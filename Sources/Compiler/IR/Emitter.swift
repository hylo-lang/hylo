import Utils

/// Val's IR emitter.
///
/// The emitter transforms well-formed, typed ASTs to a representation suitable for flow-sensitive
/// analysis and code generation.
public struct Emitter {

  /// The program being lowered.
  public let program: TypedProgram

  /// The insertion point of the emitter.
  public var insertionPoint: InsertionPoint?

  /// The state of the call stack.
  private var stack = Stack()

  /// The declaration of the receiver of the function or subscript currently emitted, if any.
  private var receiverDecl: NodeID<ParameterDecl>?

  /// Creates an emitter with a well-typed AST.
  public init(program: TypedProgram) {
    self.program = program
  }

  // MARK: Declarations

  /// Emits the Val IR of the module identified by `decl`.
  public mutating func emit(module decl: NodeID<ModuleDecl>) -> Module {
    var module = Module(decl: decl, id: program.ast[decl].name)
    for member in program.ast.topLevelDecls(decl) {
      emit(topLevel: member, into: &module)
    }
    return module
  }

  /// Emits the given top-level declaration into `module`.
  public mutating func emit(topLevel decl: AnyDeclID, into module: inout Module) {
    switch decl.kind {
    case .funDecl:
      emit(fun: NodeID(rawValue: decl.rawValue), into: &module)
    case .operatorDecl:
      break
    case .productTypeDecl:
      emit(product: NodeID(rawValue: decl.rawValue), into: &module)
    case .traitDecl:
      break
    default:
      unreachable("unexpected declaration")
    }
  }

  /// Emits the given function declaration into `module`.
  public mutating func emit(fun declID: NodeID<FunDecl>, into module: inout Module) {
    // Declare the function in the module if necessary.
    let functionID = module.getOrCreateFunction(
      from: declID,
      ast: program.ast,
      withScopeHierarchy: program.scopeHierarchy,
      withDeclTypes: program.declTypes)

    // Nothing else to do if the function has no body.
    guard let body = program.ast[declID].body else { return }

    // Create the function entry.
    assert(module.functions[functionID].blocks.isEmpty)
    let entryID = module.createBasicBlock(
      accepting: module.functions[functionID].inputs.map({ $0.type }),
      atEndOf: functionID)
    insertionPoint = InsertionPoint(endOf: entryID)

    // Configure the locals.
    var locals = DeclMap<Operand>()

    for (i, capture) in program.ast[declID].implicitParameterDecls.enumerated() {
      locals[capture.decl] = .parameter(block: entryID, index: i)
    }

    let implicitParamCount = program.ast[declID].implicitParameterDecls.count
    for (i, parameter) in program.ast[declID].parameters.enumerated() {
      locals[parameter] = .parameter(block: entryID, index: i + implicitParamCount)
    }

    // Emit the body.
    stack.push(Frame(locals: locals))
    var receiverDecl = program.ast[declID].implicitReceiverDecl
    swap(&receiverDecl, &self.receiverDecl)

    switch body {
    case .block(let stmt):
      // Emit the statements of the function.
      emit(stmt: stmt, into: &module)

    case .expr(let expr):
      // Emit the body of the function.
      let value = emitR(expr: expr, into: &module)

      // Emit stack deallocation.
      emitStackDeallocs(in: &module)

      // Emit the implicit return statement.
      if program.exprTypes[expr]! != .never {
        module.insert(ReturnInst(value: value), at: insertionPoint!)
      }

    case .bundle:
      unreachable()
    }

    swap(&receiverDecl, &self.receiverDecl)
    stack.pop()
    assert(stack.frames.isEmpty)
  }

  /// Emits the product type declaration into `module`.
  private mutating func emit(product decl: NodeID<ProductTypeDecl>, into module: inout Module) {
    for member in program.ast[decl].members {
      // Emit the method and subscript members of the type declaration.
      switch member.kind {
      case .funDecl:
        let funDecl = NodeID<FunDecl>(rawValue: member.rawValue)
        switch program.ast[funDecl].introducer.value {
        case .memberwiseInit:
          continue
        case .`init`, .deinit:
          fatalError("not implemented")
        case .fun:
          emit(fun: funDecl, into: &module)
        }

      case .subscriptDecl:
        emit(subscript: NodeID(rawValue: member.rawValue), into: &module)

      default:
        continue
      }
    }
  }

  private mutating func emit(localBinding decl: NodeID<BindingDecl>, into module: inout Module) {
    let pattern = program.ast[decl].pattern

    switch program.ast[pattern].introducer.value {
    case .var, .sinklet:
      emit(storedLocalBinding: decl, into: &module)
    case .let:
      emit(borrowedLocalBinding: decl, withCapability: .let, into: &module)
    case .inout:
      emit(borrowedLocalBinding: decl, withCapability: .inout, into: &module)
    }
  }

  private mutating func emit(
    storedLocalBinding decl: NodeID<BindingDecl>,
    into module: inout Module
  ) {
    /// The pattern of the binding being emitted.
    let pattern = program.ast[decl].pattern
    /// A table mapping an object path to its corresponding (sub-)object during destructing.
    var objects: [[Int]: Operand] = [:]
    /// The type of the initializer, if any.
    var initializerType: Type?

    // Emit the initializer, if any.
    if let initializer = program.ast[decl].initializer {
      objects[[]] = emitR(expr: initializer, into: &module)
      initializerType = program.exprTypes[initializer]!
    }

    // Allocate storage for each name introduced by the declaration.
    for (path, name) in program.ast.names(in: program.ast[pattern].subpattern) {
      let decl = program.ast[name].decl
      let declType = program.declTypes[decl]!

      let storage = module.insert(AllocStackInst(declType), at: insertionPoint!)[0]
      stack.top.allocs.append(storage)
      stack[decl] = storage

      if var rhsType = initializerType {
        // Determine the object corresponding to the current name.
        for i in 0 ..< path.count {
          // Make sure the initializer has been destructured deeply enough.
          let subpath = Array(path[0 ..< i])
          if objects[subpath] != nil { continue }

          let layout = program.abstractLayout(of: rhsType)
          rhsType = layout.storedPropertiesTypes[i]

          let wholePath = Array(path[0 ..< (i - 1)])
          let whole = objects[wholePath]!
          let parts = module.insert(
            DestructureInst(whole, as: layout.storedPropertiesTypes.map({ .object($0) })),
            at: insertionPoint!)

          for j in 0 ..< parts.count {
            objects[wholePath + [j]] = parts[j]
          }
        }

        // Borrow the storage for initialization corresponding to the current name.
        let target = module.insert(
          BorrowInst(.set, .address(declType), from: storage),
          at: insertionPoint!)[0]

        // Store the corresponding (part of) the initializer.
        module.insert(StoreInst(objects[path]!, to: target), at: insertionPoint!)
      }
    }
  }

  /// Emits borrowed bindings.
  private mutating func emit(
    borrowedLocalBinding decl: NodeID<BindingDecl>,
    withCapability capability: ProjectionType.Capability,
    into module: inout Module
  ) {
    /// The pattern of the binding being emitted.
    let pattern = program.ast[decl].pattern

    // There's nothing to do if there's no initializer.
    if let initializer = program.ast[decl].initializer {
      let source: Operand
      if (initializer.kind == .nameExpr) || (initializer.kind == .subscriptCallExpr) {
        // Emit the initializer as a l-value.
        source = emitL(expr: initializer, withCapability: capability, into: &module)
      } else {
        // emit a r-value and store it into local storage.
        let value = emitR(expr: initializer, into: &module)

        let exprType = program.exprTypes[initializer]!
        let storage = module.insert(AllocStackInst(exprType), at: insertionPoint!)[0]
        stack.top.allocs.append(storage)
        source = storage

        let target = module.insert(
          BorrowInst(.set, .address(exprType), from: storage),
          at: insertionPoint!)[0]
        module.insert(StoreInst(value, to: target), at: insertionPoint!)
      }

      for (path, name) in program.ast.names(in: program.ast[pattern].subpattern) {
        let decl = program.ast[name].decl
        let declType = program.declTypes[decl]!

        stack[decl] = module.insert(
          BorrowInst(
            capability,
            .address(declType),
            from: source,
            at: path,
            binding: decl,
            range: program.ast.ranges[decl]),
          at: insertionPoint!)[0]
      }
    }
  }

  // MARK: Statements

  /// Emits the given statement into `module` at the current insertion point.
  private mutating func emit<T: StmtID>(stmt: T, into module: inout Module) {
    switch stmt.kind {
    case .braceStmt:
      emit(brace: NodeID(rawValue: stmt.rawValue), into: &module)
    case .declStmt:
      emit(declStmt: NodeID(rawValue: stmt.rawValue), into: &module)
    case .exprStmt:
      emit(exprStmt: NodeID(rawValue: stmt.rawValue), into: &module)
    case .returnStmt:
      emit(returnStmt: NodeID(rawValue: stmt.rawValue), into: &module)
    default:
      unreachable("unexpected statement")
    }
  }

  private mutating func emit(brace stmt: NodeID<BraceStmt>, into module: inout Module) {
    stack.push()
    for s in program.ast[stmt].stmts {
      emit(stmt: s, into: &module)
    }
    emitStackDeallocs(in: &module)
    stack.pop()
  }

  private mutating func emit(declStmt stmt: NodeID<DeclStmt>, into module: inout Module) {
    switch program.ast[stmt].decl.kind {
    case .bindingDecl:
      emit(localBinding: NodeID(rawValue: program.ast[stmt].decl.rawValue), into: &module)
    default:
      unreachable("unexpected declaration")
    }
  }

  private mutating func emit(exprStmt stmt: NodeID<ExprStmt>, into module: inout Module) {
    _ = emitR(expr: program.ast[stmt].expr, into: &module)
  }

  private mutating func emit(returnStmt stmt: NodeID<ReturnStmt>, into module: inout Module) {
    let value: Operand
    if let expr = program.ast[stmt].value {
      value = emitR(expr: expr, into: &module)
    } else {
      value = .constant(.unit)
    }

    emitStackDeallocs(in: &module)
    module.insert(
      ReturnInst(value: value, range: program.ast.ranges[stmt]),
      at: insertionPoint!)
  }

  // MARK: r-values

  /// Emits `expr` as a r-value into `module` at the current insertion point.
  private mutating func emitR<T: ExprID>(expr: T, into module: inout Module) -> Operand {
    defer {
      // Mark the execution path unreachable if the computed value has type `Never`.
      if program.exprTypes[expr] == .never {
        emitStackDeallocs(in: &module)
        module.insert(UnrechableInst(), at: insertionPoint!)
      }
    }

    switch expr.kind {
    case .booleanLiteralExpr:
      return emitR(booleanLiteral: NodeID(rawValue: expr.rawValue), into: &module)
    case .condExpr:
      return emitR(cond: NodeID(rawValue: expr.rawValue), into: &module)
    case .funCallExpr:
      return emitR(funCall: NodeID(rawValue: expr.rawValue), into: &module)
    case .integerLiteralExpr:
      return emitR(integerLiteral: NodeID(rawValue: expr.rawValue), into: &module)
    case .nameExpr:
      return emitR(name: NodeID(rawValue: expr.rawValue), into: &module)
    case .sequenceExpr:
      return emitR(sequence: NodeID(rawValue: expr.rawValue), into: &module)
    default:
      unreachable("unexpected expression")
    }
  }

  private mutating func emitR(
    booleanLiteral expr: NodeID<BooleanLiteralExpr>,
    into module: inout Module
  ) -> Operand {
    let value = Operand.constant(.integer(IntegerConstant(
      bitPattern: BitPattern(pattern: program.ast[expr].value ? 1 : 0, width: 1))))

    let boolType = ProductType(standardLibraryTypeNamed: "Bool", ast: program.ast)!
    return module.insert(
      RecordInst(objectType: .object(.product(boolType)), operands: [value]),
      at: insertionPoint!)[0]
  }

  private mutating func emitR(
    cond expr: NodeID<CondExpr>,
    into module: inout Module
  ) -> Operand {
    let functionID = insertionPoint!.block.function

    // If the expression is supposed to return a value, allocate storage for it.
    var resultStorage: Operand?
    if let type = program.exprTypes[expr], type != .unit {
      resultStorage = module.insert(AllocStackInst(type), at: insertionPoint!)[0]
      stack.top.allocs.append(resultStorage!)
    }

    // Emit the condition(s).
    var alt: Block.ID?

    for item in program.ast[expr].condition {
      let success = module.createBasicBlock(atEndOf: functionID)
      let failure = module.createBasicBlock(atEndOf: functionID)
      alt = failure

      switch item {
      case .expr(let itemExpr):
        // Evaluate the condition in the current block.
        var condition = emitL(expr: itemExpr, withCapability: .let, into: &module)
        condition = module.insert(
          BorrowInst(.let, .address(.builtin(.i(1))), from: condition, at: [0]),
          at: insertionPoint!)[0]
        condition = module.insert(
          CallInst(
            returnType: .object(.builtin(.i(1))),
            conventions: [.let, .let],
            callee: .constant(.builtin(BuiltinFunctionRef["i1_copy"]!)),
            arguments: [condition]),
          at: insertionPoint!)[0]

        module.insert(
          CondBranchInst(
            condition: condition,
            targetIfTrue: success,
            targetIfFalse: failure),
          at: insertionPoint!)
        insertionPoint = InsertionPoint(endOf: success)

      case .decl:
        fatalError("not implemented")
      }
    }

    let continuation = module.createBasicBlock(atEndOf: functionID)

    // Emit the success branch.
    // Note: the insertion pointer is already set in the corresponding block.
    switch program.ast[expr].success {
    case .expr(let thenExpr):
      stack.push()
      let value = emitR(expr: thenExpr, into: &module)
      if let target = resultStorage {
        let target = module.insert(
          BorrowInst(.set, .address(program.exprTypes[expr]!), from: target),
          at: insertionPoint!)[0]
        module.insert(StoreInst(value, to: target), at: insertionPoint!)
      }
      emitStackDeallocs(in: &module)
      stack.pop()

    case .block:
      fatalError("not implemented")
    }
    module.insert(BranchInst(target: continuation), at: insertionPoint!)

    // Emit the failure branch.
    insertionPoint = InsertionPoint(endOf: alt!)
    switch program.ast[expr].failure {
    case .expr(let elseExpr):
      stack.push()
      let value = emitR(expr: elseExpr, into: &module)
      if let target = resultStorage {
        let target = module.insert(
          BorrowInst(.set, .address(program.exprTypes[expr]!), from: target),
          at: insertionPoint!)[0]
        module.insert(StoreInst(value, to: target), at: insertionPoint!)
      }
      emitStackDeallocs(in: &module)
      stack.pop()

    case .block:
      fatalError("not implemented")

    case nil:
      break
    }
    module.insert(BranchInst(target: continuation), at: insertionPoint!)

    // Emit the value of the expression.
    insertionPoint = InsertionPoint(endOf: continuation)
    if let source = resultStorage {
      return module.insert(
        LoadInst(LoweredType(lowering: program.exprTypes[expr]!), from: source),
        at: insertionPoint!)[0]
    } else {
      return .constant(.unit)
    }
  }

  private mutating func emitR(
    funCall expr: NodeID<FunCallExpr>,
    into module: inout Module
  ) -> Operand {
    let calleeType = LambdaType(converting: program.exprTypes[program.ast[expr].callee]!)!

    // Determine the callee's convention.
    var conventions: [PassingConvention]
    switch calleeType.receiverEffect {
    case .inout   : conventions = [.inout]
    case .sink    : conventions = [.sink]
    case .yielded : conventions = [.yielded]
    case nil      : conventions = [.let]
    }

    // Arguments are evaluated first, from left to right.
    var arguments: [Operand] = []

    for (parameter, argument) in zip(calleeType.inputs, program.ast[expr].arguments) {
      let parameterType = ParameterType(converting: parameter.type)!
      conventions.append(parameterType.convention)

      switch parameterType.convention {
      case .let:
        arguments.append(emitL(expr: argument.value, withCapability: .let, into: &module))
      case .inout:
        arguments.append(emitL(expr: argument.value, withCapability: .inout, into: &module))
      case .set:
        arguments.append(emitL(expr: argument.value, withCapability: .set, into: &module))
      case .sink:
        arguments.append(emitR(expr: argument.value, into: &module))
      case .yielded:
        fatalError("not implemented")
      }
    }

    // If the callee is a name expression referring to the declaration of a function capture-less
    // function, it is interpreted as a direct function reference. Otherwise, it is evaluated as a
    // function object the arguments.
    let callee: Operand

    if let calleeID = NodeID<NameExpr>(converting: program.ast[expr].callee) {
      switch program.referredDecls[calleeID] {
      case .direct(let calleeDeclID) where calleeDeclID.kind == .builtinDecl:
        // Callee refers to a built-in function.
        assert(calleeType.environment == .unit)
        callee = .constant(.builtin(BuiltinFunctionRef(
          name: program.ast[calleeID].name.value.stem,
          type: .address(.lambda(calleeType)))))

      case .direct(let calleeDeclID) where calleeDeclID.kind == .funDecl:
        // Callee is a direct reference to a function declaration.
        switch (program.ast[calleeDeclID] as! FunDecl).introducer.value {
        case .memberwiseInit:
          // The function is a memberwise initializer. In that case, the whole call expression is
          // lowered as a `record` instruction.
          return module.insert(
            RecordInst(objectType: .object(program.exprTypes[expr]!), operands: arguments),
            at: insertionPoint!)[0]

        case .`init`:
          // The function is a custom initializer. TODO
          fatalError("not implemented")

        default:
          // TODO: handle captures
          let locator = program.locator(identifying: calleeDeclID)
          callee = .constant(.function(FunctionRef(
            name: locator.mangled,
            type: .address(.lambda(calleeType)))))
        }

      case .member(let calleeDeclID) where calleeDeclID.kind == .funDecl:
        // Callee is a member reference to a method.
        let receiverType = calleeType.captures[0].type

        // Add the receiver to the arguments.
        if case .projection(let type) = receiverType {
          // The receiver as a borrowing convention.
          conventions.insert(PassingConvention(matching: type.capability), at: 1)

          switch program.ast[calleeID].domain {
          case .none:
            let receiver = module.insert(
              BorrowInst(type.capability, .address(type.base), from: stack[receiverDecl!]!),
              at: insertionPoint!)[0]
            arguments.insert(receiver, at: 0)

          case .expr(let receiverID):
            let receiver = emitL(expr: receiverID, withCapability: type.capability, into: &module)
            arguments.insert(receiver, at: 0)

          case .type:
            fatalError("not implemented")

          case .implicit:
            unreachable()
          }
        } else {
          // The receiver is consumed.
          conventions.insert(.sink, at: 1)

          switch program.ast[calleeID].domain {
          case .none:
            let receiver = module.insert(
              LoadInst(.object(receiverType), from: stack[receiverDecl!]!),
              at: insertionPoint!)[0]
            arguments.insert(receiver, at: 0)

          case .expr(let receiverID):
            arguments.insert(emitR(expr: receiverID, into: &module), at: 0)

          case .type:
            fatalError("not implemented")

          case .implicit:
            unreachable()
          }
        }

        // Emit the function reference.
        let locator = program.locator(identifying: calleeDeclID)
        callee = .constant(.function(FunctionRef(
          name: locator.mangled,
          type: .address(.lambda(calleeType)))))

      default:
        // Evaluate the callee as a function object.
        callee = emitR(expr: program.ast[expr].callee, into: &module)
      }
    } else {
      // Evaluate the callee as a function object.
      callee = emitR(expr: program.ast[expr].callee, into: &module)
    }

    return module.insert(
      CallInst(
        returnType: .object(program.exprTypes[expr]!),
        conventions: conventions,
        callee: callee,
        arguments: arguments),
      at: insertionPoint!)[0]
  }

  private mutating func emitR(
    integerLiteral expr: NodeID<IntegerLiteralExpr>,
    into module: inout Module
  ) -> Operand {
    guard case .product(let type) = program.exprTypes[expr]! else { unreachable() }

    switch type.name.value {
    case "Int":
      let bits = BitPattern(fromDecimal: program.ast[expr].value)!.resized(to: 64)
      let value = IntegerConstant(bitPattern: bits)
      return module.insert(
        RecordInst(objectType: .object(.product(type)), operands: [.constant(.integer(value))]),
        at: insertionPoint!)[0]

    default:
      unreachable("unexpected numeric type")
    }
  }

  private mutating func emitR(
    name expr: NodeID<NameExpr>,
    into module: inout Module
  ) -> Operand {
    switch program.referredDecls[expr]! {
    case .direct(let declID):
      // Lookup for a local symbol.
      if let source = stack[declID] {
        return module.insert(
          LoadInst(.object(program.exprTypes[expr]!), from: source),
          at: insertionPoint!)[0]
      }

      fatalError("not implemented")

    case .member:
      fatalError("not implemented")
    }
  }

  private mutating func emitR(
    sequence expr: NodeID<SequenceExpr>,
    into module: inout Module
  ) -> Operand {
    guard case .root(let root) = program.ast[expr] else { unreachable() }
    return emitR(expr: root, into: &module)
  }

  // MARK: l-values

  /// Emits `expr` as a l-value with the specified capability into `module` at the current
  /// insertion point.
  private mutating func emitL<T: ExprID>(
    expr: T,
    withCapability capability: ProjectionType.Capability,
    into module: inout Module
  ) -> Operand {
    switch expr.kind {
    case .nameExpr:
      return emitL(
        name: NodeID(rawValue: expr.rawValue), withCapability: capability, into: &module)

    case .subscriptCallExpr:
      fatalError("not implemented")

    default:
      let exprType = program.exprTypes[expr]!

      let value = emitR(expr: expr, into: &module)
      let storage = module.insert(AllocStackInst(exprType), at: insertionPoint!)[0]
      stack.top.allocs.append(storage)

      let target = module.insert(
        BorrowInst(.set, .address(exprType), from: storage),
        at: insertionPoint!)[0]
      module.insert(StoreInst(value, to: target), at: insertionPoint!)

      return module.insert(
        BorrowInst(capability, .address(exprType), from: storage),
        at: insertionPoint!)[0]
    }
  }

  private mutating func emitL(
    name expr: NodeID<NameExpr>,
    withCapability capability: ProjectionType.Capability,
    into module: inout Module
  ) -> Operand {
    switch program.referredDecls[expr]! {
    case .direct(let declID):
      // Lookup for a local symbol.
      if let source = stack[declID] {
        return module.insert(
          BorrowInst(capability, .address(program.exprTypes[expr]!), from: source),
          at: insertionPoint!)[0]
      }

      fatalError("not implemented")

    case .member(let declID):
      // Emit the receiver.
      let receiver: Operand

      switch program.ast[expr].domain {
      case .none:
        receiver = stack[receiverDecl!]!
      case .implicit:
        fatalError("not implemented")
      case .expr(let receiverID):
        receiver = emitL(expr: receiverID, withCapability: capability, into: &module)
      case .type:
        fatalError("not implemented")
      }

      // Emit the bound member.
      switch declID.kind {
      case .varDecl:
        let declID = NodeID<VarDecl>(rawValue: declID.rawValue)
        let layout = program.abstractLayout(of: module.type(of: receiver).astType)
        let memberIndex = layout.storedPropertiesIndices[program.ast[declID].name]!

        // If the lowered receiver is a borrow instruction, modify it in place so that it targets
        // the requested stored member. Otherwise, emit a reborrow.
        if let id = receiver.inst,
           let receiverInst = module[id.function][id.block][id.address] as? BorrowInst
        {
          module[id.function][id.block][id.address] = BorrowInst(
            capability,
            .address(program.exprTypes[expr]!),
            from: receiverInst.location,
            at: receiverInst.path + [memberIndex])
          return receiver
        } else {
          let member = BorrowInst(
            capability,
            .address(program.exprTypes[expr]!),
            from: receiver,
            at: [memberIndex])
          return module.insert(member, at: insertionPoint!)[0]
        }

      default:
        fatalError("not implemented")
      }
    }

    fatalError()
  }

  // MARK: Helpers

  private mutating func emitStackDeallocs(in module: inout Module) {
    while let alloc = stack.top.allocs.popLast() {
      module.insert(DeallocStackInst(alloc), at: insertionPoint!)
    }
  }

}

fileprivate extension Emitter {

  /// A type describing the local variables and allocations of a stack frame.
  struct Frame {

    /// The local variables in scope.
    var locals = DeclMap<Operand>()

    /// The stack allocations, in FILO order.
    var allocs: [Operand] = []

  }

  /// A type describing the state of the call stack during lowering.
  struct Stack {

    /// The frames in the stack, in FILO order.
    var frames: [Frame] = []

    /// Accesses the top frame, assuming the stack is not empty.
    var top: Frame {
      get {
        frames[frames.count - 1]
      }
      _modify {
        yield &frames[frames.count - 1]
      }
    }

    /// Accesses the operand assigned `decl`, assuming the stack is not empty.
    subscript<T: DeclID>(decl: T) -> Operand? {
      get {
        for frame in frames.reversed() {
          if let operand = frame.locals[decl] { return operand }
        }
        return nil
      }
      set {
        top.locals[decl] = newValue
      }
    }

    /// Pushes a new frame.
    mutating func push(_ newFrame: Frame = Frame()) {
      frames.append(newFrame)
    }

    /// Pops a frame, assuming the stack is not empty.
    mutating func pop() {
      precondition(top.allocs.isEmpty, "stack leak")
      frames.removeLast()
    }

  }

}
