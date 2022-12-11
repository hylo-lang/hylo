import BigInt
import Utils

/// Val's IR emitter.
///
/// The emitter transforms well-formed, typed ASTs to a representation suitable for flow-sensitive
/// analysis and code generation.
public struct Emitter {

  /// The program being lowered.
  public let program: TypedProgram

  /// The insertion point of the emitter.
  private var insertionPoint: InsertionPoint?

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
    var module = Module(decl: decl, name: program.ast[decl].name)
    for member in program.ast.topLevelDecls(decl) {
      emit(topLevel: member, into: &module)
    }
    return module
  }

  /// Emits the given top-level declaration into `module`.
  public mutating func emit(topLevel decl: AnyDeclID, into module: inout Module) {
    switch decl.kind {
    case FunctionDecl.self:
      emit(function: NodeID(rawValue: decl.rawValue), into: &module)
    case OperatorDecl.self:
      break
    case ProductTypeDecl.self:
      emit(product: NodeID(rawValue: decl.rawValue), into: &module)
    case TraitDecl.self:
      break
    default:
      unreachable("unexpected declaration")
    }
  }

  /// Emits the given function declaration into `module`.
  public mutating func emit(function declID: NodeID<FunctionDecl>, into module: inout Module) {
    // Declare the function in the module if necessary.
    let functionID = module.getOrCreateFunction(correspondingTo: declID, program: program)

    // Nothing else to do if the function has no body.
    guard let body = program.ast[declID].body else { return }

    // Create the function entry.
    assert(module.functions[functionID].blocks.isEmpty)
    let entryID = module.createBasicBlock(
      accepting: module.functions[functionID].inputs.map({ $0.type }),
      atEndOf: functionID)
    insertionPoint = InsertionPoint(endOf: entryID)

    // Configure the locals.
    var locals = DeclProperty<Operand>()

    let explicitCaptures = program.ast[declID].explicitCaptures
    for (i, capture) in explicitCaptures.enumerated() {
      locals[capture] = .parameter(block: entryID, index: i)
    }

    let implicitCaptures = program.implicitCaptures[declID]!
    for (i, capture) in implicitCaptures.enumerated() {
      locals[capture.decl] = .parameter(block: entryID, index: i + explicitCaptures.count)
    }

    var implicitParameterCount = explicitCaptures.count + implicitCaptures.count
    if let receiver = program.ast[declID].receiver {
      locals[receiver] = .parameter(block: entryID, index: implicitParameterCount)
      implicitParameterCount += 1
    }

    for (i, parameter) in program.ast[declID].parameters.enumerated() {
      locals[parameter] = .parameter(block: entryID, index: i + implicitParameterCount)
    }

    // Emit the body.
    stack.push(Frame(locals: locals))
    var receiverDecl = program.ast[declID].receiver
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
    }

    swap(&receiverDecl, &self.receiverDecl)
    stack.pop()
    assert(stack.frames.isEmpty)
  }

  /// Emits the given subscript declaration into `module`.
  public mutating func emit(
    subscript declID: NodeID<SubscriptDecl>,
    into module: inout Module
  ) {
    fatalError("not implemented")
  }

  /// Emits the product type declaration into `module`.
  private mutating func emit(product decl: NodeID<ProductTypeDecl>, into module: inout Module) {
    for member in program.ast[decl].members {
      // Emit the member functions and subscripts of the type declaration.
      switch member.kind {
      case FunctionDecl.self:
        emit(function: NodeID(rawValue: member.rawValue), into: &module)

      case InitializerDecl.self:
        let d = NodeID<InitializerDecl>(rawValue: member.rawValue)
        if program.ast[d].introducer.value == .memberwiseInit { continue }
        fatalError("not implemented")

      case SubscriptDecl.self:
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
    /// A map from object path to its corresponding (sub-)object during destruction.
    var objects: [[Int]: Operand] = [:]
    /// The type of the initializer, if any.
    var initializerType: AnyType?

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
        for i in 0..<path.count {
          // Make sure the initializer has been destructured deeply enough.
          let subpath = Array(path[0..<i])
          if objects[subpath] != nil { continue }

          let layout = program.abstractLayout(of: rhsType)
          rhsType = layout.storedPropertiesTypes[i]

          let wholePath = Array(path[0..<(i - 1)])
          let whole = objects[wholePath]!
          let parts = module.insert(
            DestructureInst(whole, as: layout.storedPropertiesTypes.map({ .object($0) })),
            at: insertionPoint!)

          for j in 0..<parts.count {
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
    withCapability capability: RemoteType.Capability,
    into module: inout Module
  ) {
    /// The pattern of the binding being emitted.
    let pattern = program.ast[decl].pattern

    // There's nothing to do if there's no initializer.
    if let initializer = program.ast[decl].initializer {
      let source: Operand
      if (initializer.kind == NameExpr.self) || (initializer.kind == SubscriptCallExpr.self) {
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

        stack[decl] =
          module.insert(
            BorrowInst(
              capability,
              .address(declType),
              from: source,
              at: path,
              binding: decl,
              range: program.ast[decl].origin),
            at: insertionPoint!)[0]
      }
    }
  }

  // MARK: Statements

  /// Emits the given statement into `module` at the current insertion point.
  private mutating func emit<T: StmtID>(stmt: T, into module: inout Module) {
    switch stmt.kind {
    case AssignStmt.self:
      emit(assign: NodeID(rawValue: stmt.rawValue), into: &module)
    case BraceStmt.self:
      emit(brace: NodeID(rawValue: stmt.rawValue), into: &module)
    case DeclStmt.self:
      emit(declStmt: NodeID(rawValue: stmt.rawValue), into: &module)
    case ExprStmt.self:
      emit(exprStmt: NodeID(rawValue: stmt.rawValue), into: &module)
    case ReturnStmt.self:
      emit(returnStmt: NodeID(rawValue: stmt.rawValue), into: &module)
    default:
      unreachable("unexpected statement")
    }
  }

  private mutating func emit(assign stmt: NodeID<AssignStmt>, into module: inout Module) {
    let rhs = emitR(expr: program.ast[stmt].right, into: &module)
    // FIXME: Should request the capability 'set or inout'.
    let lhs = emitL(expr: program.ast[stmt].left, withCapability: .set, into: &module)
    _ = module.insert(StoreInst(rhs, to: lhs), at: insertionPoint!)
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
    case BindingDecl.self:
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
      value = .constant(.void)
    }

    emitStackDeallocs(in: &module)
    module.insert(
      ReturnInst(value: value, range: program.ast[stmt].origin),
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
    case BooleanLiteralExpr.self:
      return emitR(booleanLiteral: NodeID(rawValue: expr.rawValue), into: &module)
    case CondExpr.self:
      return emitR(cond: NodeID(rawValue: expr.rawValue), into: &module)
    case FunCallExpr.self:
      return emitR(funCall: NodeID(rawValue: expr.rawValue), into: &module)
    case IntegerLiteralExpr.self:
      return emitR(integerLiteral: NodeID(rawValue: expr.rawValue), into: &module)
    case NameExpr.self:
      return emitR(name: NodeID(rawValue: expr.rawValue), into: &module)
    case SequenceExpr.self:
      return emitR(sequence: NodeID(rawValue: expr.rawValue), into: &module)
    default:
      unreachable("unexpected expression")
    }
  }

  private mutating func emitR(
    booleanLiteral expr: NodeID<BooleanLiteralExpr>,
    into module: inout Module
  ) -> Operand {
    let value = Operand.constant(
      .integer(IntegerConstant(program.ast[expr].value ? 1 : 0, bitWidth: 1)))

    let boolType = program.ast.coreType(named: "Bool")!
    return module.insert(
      RecordInst(objectType: .object(boolType), operands: [value]),
      at: insertionPoint!)[0]
  }

  private mutating func emitR(
    cond expr: NodeID<CondExpr>,
    into module: inout Module
  ) -> Operand {
    let functionID = insertionPoint!.block.function

    // If the expression is supposed to return a value, allocate storage for it.
    var resultStorage: Operand?
    if let type = program.exprTypes[expr], type != .void {
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
        condition =
          module.insert(
            BorrowInst(.let, .address(BuiltinType.i(1)), from: condition, at: [0]),
            at: insertionPoint!)[0]
        condition =
          module.insert(
            CallInst(
              returnType: .object(BuiltinType.i(1)),
              calleeConvention: .let,
              callee: .constant(.builtin(BuiltinFunctionRef["i1_copy"]!)),
              argumentConventions: [.let],
              arguments: [condition]),
            at: insertionPoint!)[0]

        module.insert(
          CondBranchInst(
            condition: condition,
            targetIfTrue: success,
            targetIfFalse: failure,
            range: nil),
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
      return .constant(.void)
    }
  }

  private mutating func emitR(
    funCall expr: NodeID<FunCallExpr>,
    into module: inout Module
  ) -> Operand {
    let calleeType = program.exprTypes[program.ast[expr].callee]!.base as! LambdaType

    // Determine the callee's convention.
    let calleeConvention = calleeType.receiverEffect.map(
      default: .let,
      PassingConvention.init(matching:))

    // Arguments are evaluated first, from left to right.
    var argumentConventions: [PassingConvention] = []
    var arguments: [Operand] = []

    for (parameter, argument) in zip(calleeType.inputs, program.ast[expr].arguments) {
      let parameterType = parameter.type.base as! ParameterType
      argumentConventions.append(parameterType.convention)
      arguments.append(emit(argument: argument.value, to: parameterType, into: &module))
    }

    // If the callee is a name expression referring to the declaration of a function capture-less
    // function, it is interpreted as a direct function reference. Otherwise, it is evaluated as a
    // function object the arguments.
    let callee: Operand

    if let calleeID = NodeID<NameExpr>(program.ast[expr].callee) {
      switch program.referredDecls[calleeID] {
      case .direct(let calleeDecl) where calleeDecl.kind == BuiltinDecl.self:
        // Callee refers to a built-in function.
        assert(calleeType.environment == .void)
        callee = .constant(
          .builtin(
            BuiltinFunctionRef(
              name: program.ast[calleeID].name.value.stem,
              type: .address(calleeType))))

      case .direct(let calleeDecl) where calleeDecl.kind == FunctionDecl.self:
        // Callee is a direct reference to a function or initializer declaration.
        // TODO: handle captures
        callee = .constant(
          .function(
            FunctionRef(
              name: DeclLocator(identifying: calleeDecl, in: program).mangled,
              type: .address(calleeType))))

      case .direct(let calleeDecl) where calleeDecl.kind == InitializerDecl.self:
        let d = NodeID<InitializerDecl>(rawValue: calleeDecl.rawValue)
        switch program.ast[d].introducer.value {
        case .`init`:
          // TODO: The function is a custom initializer.
          fatalError("not implemented")

        case .memberwiseInit:
          // The function is a memberwise initializer. In that case, the whole call expression is
          // lowered as a `record` instruction.
          return module.insert(
            RecordInst(objectType: .object(program.exprTypes[expr]!), operands: arguments),
            at: insertionPoint!)[0]
        }

      case .member(let calleeDecl) where calleeDecl.kind == FunctionDecl.self:
        // Callee is a member reference to a function or method.
        let receiverType = calleeType.captures[0].type

        // Add the receiver to the arguments.
        if let type = RemoteType(receiverType) {
          // The receiver as a borrowing convention.
          argumentConventions.insert(PassingConvention(matching: type.capability), at: 0)

          switch program.ast[calleeID].domain {
          case .none:
            let receiver = module.insert(
              BorrowInst(type.capability, .address(type.base), from: stack[receiverDecl!]!),
              at: insertionPoint!)[0]
            arguments.insert(receiver, at: 0)

          case .expr(let receiverID):
            let receiver = emitL(expr: receiverID, withCapability: type.capability, into: &module)
            arguments.insert(receiver, at: 0)

          case .implicit:
            unreachable()
          }
        } else {
          // The receiver is consumed.
          argumentConventions.insert(.sink, at: 0)

          switch program.ast[calleeID].domain {
          case .none:
            let receiver = module.insert(
              LoadInst(.object(receiverType), from: stack[receiverDecl!]!),
              at: insertionPoint!)[0]
            arguments.insert(receiver, at: 0)

          case .expr(let receiverID):
            arguments.insert(emitR(expr: receiverID, into: &module), at: 0)

          case .implicit:
            unreachable()
          }
        }

        // Emit the function reference.
        callee = .constant(
          .function(
            FunctionRef(
              name: DeclLocator(identifying: calleeDecl, in: program).mangled,
              type: .address(calleeType))))

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
        calleeConvention: calleeConvention,
        callee: callee,
        argumentConventions: argumentConventions,
        arguments: arguments),
      at: insertionPoint!)[0]
  }

  private mutating func emitR(
    integerLiteral expr: NodeID<IntegerLiteralExpr>,
    into module: inout Module
  ) -> Operand {
    let type = program.exprTypes[expr]!.base as! ProductType

    // Determine the bit width of the value.
    let bitWidth: Int
    switch type.name.value {
    case "Int": bitWidth = 64
    case "Int32": bitWidth = 32
    default:
      unreachable("unexpected numeric type")
    }

    // Convert the literal into a bit pattern.
    let bits: BigUInt
    let s = program.ast[expr].value
    if s.starts(with: "0x") {
      bits = BigUInt(s.dropFirst(2), radix: 16)!
    } else {
      bits = BigUInt(s.dropFirst(2))!
    }

    // Emit the constant integer.
    let value = IntegerConstant(bits, bitWidth: bitWidth)
    return module.insert(
      RecordInst(objectType: .object(type), operands: [.constant(.integer(value))]),
      at: insertionPoint!)[0]
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
    emit(.sink, foldedSequence: program.foldedSequenceExprs[expr]!, into: &module)
  }

  private mutating func emit(
    _ convention: PassingConvention,
    foldedSequence expr: FoldedSequenceExpr,
    into module: inout Module
  ) -> Operand {
    switch expr {
    case .infix(let callee, let lhs, let rhs):
      let calleeType = program.exprTypes[callee.expr]!.base as! LambdaType

      // Emit the operands, starting with RHS.
      let rhsType = calleeType.inputs[0].type.base as! ParameterType
      let rhsOperand = emit(rhsType.convention, foldedSequence: rhs, into: &module)

      let lhsConvention: PassingConvention
      let lhsOperand: Operand
      if let lhsType = RemoteType(calleeType.captures[0].type) {
        lhsConvention = PassingConvention(matching: lhsType.capability)
        lhsOperand = emit(lhsConvention, foldedSequence: lhs, into: &module)
      } else {
        lhsConvention = .sink
        lhsOperand = emit(.sink, foldedSequence: lhs, into: &module)
      }

      // Create the callee's value.
      let calleeOperand: Operand
      switch program.referredDecls[callee.expr] {
      case .member(let calleeDecl) where calleeDecl.kind == FunctionDecl.self:
        calleeOperand = Operand.constant(
          .function(
            FunctionRef(
              name: DeclLocator(identifying: calleeDecl, in: program).mangled,
              type: .address(calleeType))))

      default:
        unreachable()
      }

      // Emit the call.
      return module.insert(
        CallInst(
          returnType: .object(calleeType.output),
          calleeConvention: .let,
          callee: calleeOperand,
          argumentConventions: [lhsConvention, rhsType.convention],
          arguments: [lhsOperand, rhsOperand]),
        at: insertionPoint!)[0]

    case .leaf(let expr):
      switch convention {
      case .let:
        return emitL(expr: expr, withCapability: .let, into: &module)
      case .inout:
        return emitL(expr: expr, withCapability: .inout, into: &module)
      case .set:
        return emitL(expr: expr, withCapability: .set, into: &module)
      case .sink:
        return emitR(expr: expr, into: &module)
      case .yielded:
        fatalError("not implemented")
      }
    }
  }

  private mutating func emit(
    argument expr: AnyExprID,
    to parameterType: ParameterType,
    into module: inout Module
  ) -> Operand {
    switch parameterType.convention {
    case .let:
      return emitL(expr: expr, withCapability: .let, into: &module)
    case .inout:
      return emitL(expr: expr, withCapability: .inout, into: &module)
    case .set:
      return emitL(expr: expr, withCapability: .set, into: &module)
    case .sink:
      return emitR(expr: expr, into: &module)
    case .yielded:
      fatalError("not implemented")
    }
  }

  /// Emits `expr` as an operand suitable to be the callee of a `CallInst`, updating `conventions`
  /// and `arguments` if `expr` refers to a bound member function.
  ///
  /// - Requires: `expr` has a lambda type.
  private mutating func emitCallee(
    _ expr: AnyExprID,
    conventions: inout [PassingConvention],
    arguments: inout [Operand],
    into module: inout Module
  ) -> Operand {
    let calleeType = program.exprTypes[expr]!.base as! LambdaType

    // If the callee is a name expression referring to the declaration of a capture-less function,
    // it is interpreted as a direct function reference.
    if let nameExpr = NodeID<NameExpr>(expr) {
      switch program.referredDecls[nameExpr] {
      case .direct(let calleeDecl) where calleeDecl.kind == BuiltinDecl.self:
        // Callee refers to a built-in function.
        assert(calleeType.environment == .void)
        return .constant(
          .builtin(
            BuiltinFunctionRef(
              name: program.ast[nameExpr].name.value.stem,
              type: .address(calleeType))))

      case .direct(let calleeDecl) where calleeDecl.kind == FunctionDecl.self:
        // Callee is a direct reference to a function or initializer declaration.
        // TODO: handle captures
        return .constant(
          .function(
            FunctionRef(
              name: DeclLocator(identifying: calleeDecl, in: program).mangled,
              type: .address(calleeType))))

      case .direct(let calleeDecl) where calleeDecl.kind == InitializerDecl.self:
        let d = NodeID<InitializerDecl>(rawValue: nameExpr.rawValue)
        switch program.ast[d].introducer.value {
        case .`init`:
          // The function is a custom initializer.
          fatalError("not implemented")

        case .memberwiseInit:
          // The function is a memberwise initializer.
          fatalError("not implemented")
        }

      case .member(let calleeDecl) where calleeDecl.kind == FunctionDecl.self:
        // Callee is a member reference to a function or method.
        let receiverType = calleeType.captures[0].type

        // Add the receiver to the arguments.
        if let type = RemoteType(receiverType) {
          // The receiver has a borrowing convention.
          conventions.insert(PassingConvention(matching: type.capability), at: 1)

          switch program.ast[nameExpr].domain {
          case .none:
            let receiver = module.insert(
              BorrowInst(type.capability, .address(type.base), from: stack[receiverDecl!]!),
              at: insertionPoint!)[0]
            arguments.insert(receiver, at: 0)

          case .expr(let receiverID):
            let receiver = emitL(expr: receiverID, withCapability: type.capability, into: &module)
            arguments.insert(receiver, at: 0)

          case .implicit:
            unreachable()
          }
        } else {
          // The receiver is consumed.
          conventions.insert(.sink, at: 1)

          switch program.ast[nameExpr].domain {
          case .none:
            let receiver = module.insert(
              LoadInst(.object(receiverType), from: stack[receiverDecl!]!),
              at: insertionPoint!)[0]
            arguments.insert(receiver, at: 0)

          case .expr(let receiverID):
            arguments.insert(emitR(expr: receiverID, into: &module), at: 0)

          case .implicit:
            unreachable()
          }
        }

        // Emit the function reference.
        return .constant(
          .function(
            FunctionRef(
              name: DeclLocator(identifying: calleeDecl, in: program).mangled,
              type: .address(calleeType))))

      default:
        // Callee is a lambda.
        break
      }
    }

    // Otherwise, by default, a callee is evaluated as a function object.
    return emitR(expr: expr, into: &module)
  }

  // MARK: l-values

  /// Emits `expr` as a l-value with the specified capability into `module` at the current
  /// insertion point.
  private mutating func emitL<T: ExprID>(
    expr: T,
    withCapability capability: RemoteType.Capability,
    into module: inout Module
  ) -> Operand {
    switch expr.kind {
    case NameExpr.self:
      return emitL(
        name: NodeID(rawValue: expr.rawValue), withCapability: capability, into: &module)

    case SubscriptCallExpr.self:
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
    withCapability capability: RemoteType.Capability,
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
      }

      // Emit the bound member.
      switch declID.kind {
      case VarDecl.self:
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

extension Emitter {

  /// A type describing the local variables and allocations of a stack frame.
  fileprivate struct Frame {

    /// The local variables in scope.
    var locals = DeclProperty<Operand>()

    /// The stack allocations, in FILO order.
    var allocs: [Operand] = []

  }

  /// A type describing the state of the call stack during lowering.
  fileprivate struct Stack {

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
