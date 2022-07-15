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

  /// The local variables in scope.
  private var locals = DeclMap<Operand>()

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
    for member in program.ast[decl].members {
      emit(topLevel: member, into: &module)
    }
    return module
  }

  /// Emits the given top-level declaration into `module`.
  public mutating func emit(topLevel decl: AnyDeclID, into module: inout Module) {
    switch decl.kind {
    case .funDecl:
      emit(fun: NodeID(unsafeRawValue: decl.rawValue), into: &module)
    case .operatorDecl:
      break
    case .productTypeDecl:
      emit(product: NodeID(unsafeRawValue: decl.rawValue), into: &module)
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

    // Emit the function's body.
    var receiverDecl = program.ast[declID].implicitReceiverDecl
    swap(&receiverDecl, &self.receiverDecl)
    swap(&locals, &self.locals)

    switch program.ast[declID].body!.value {
    case .block(let stmt):
      emit(stmt: stmt, into: &module)

    case .expr(let expr):
      let value = emitR(expr: expr, into: &module)
      if program.exprTypes[expr]! != .never {
        module.insert(ReturnInst(value: value), at: insertionPoint!)
      }

    case .bundle:
      unreachable()
    }

    swap(&locals, &self.locals)
    swap(&receiverDecl, &self.receiverDecl)
  }

  /// Emits the product type declaration into `module`.
  private mutating func emit(product declID: NodeID<ProductTypeDecl>, into module: inout Module) {
    for member in program.ast[declID].members {
      // Emit the method and subscript members of the type declaration.
      switch member.kind {
      case .funDecl:
        let funDeclID = NodeID<FunDecl>(unsafeRawValue: member.rawValue)
        switch program.ast[funDeclID].introducer.value {
        case .memberwiseInit:
          continue
        case .`init`, .deinit:
          fatalError("not implemented")
        case .fun:
          emit(fun: funDeclID, into: &module)
        }

      case .subscriptDecl:
        fatalError("not implemented")

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
    let pattern = program.ast[decl].pattern

    // Emit the initializer, if any.
    let initializer = program.ast[decl].initializer.map({ emitR(expr: $0, into: &module) })

    // Allocate storage for each name introduced by the declaration.
    for (path, name) in program.ast.names(in: program.ast[pattern].subpattern) {
      let decl = program.ast[name].decl
      let declType = program.declTypes[decl]!

      let storage = module.insert(AllocStackInst(declType), at: insertionPoint!)
      locals[decl] = storage

      if let source = initializer {
        let target = module.insert(
          BorrowInst(type: .address(declType), capability: .set, value: storage, path: []),
          at: insertionPoint!)

        if path.isEmpty {
          module.insert(StoreInst(object: source, target: target), at: insertionPoint!)
        } else {
          let member = module.insert(
            TakeMemberInst(type: .object(declType), value: source, path: path),
            at: insertionPoint!)
          module.insert(StoreInst(object: member, target: target), at: insertionPoint!)
        }
      }
    }
  }

  /// Emits borrowed bindings.
  private mutating func emit(
    borrowedLocalBinding decl: NodeID<BindingDecl>,
    withCapability capability: ProjectionType.Capability,
    into module: inout Module
  ) {
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
        let storage = module.insert(AllocStackInst(exprType), at: insertionPoint!)
        source = storage

        let target = module.insert(
          BorrowInst(type: .address(exprType), capability: .set, value: storage, path: []),
          at: insertionPoint!)
        module.insert(StoreInst(object: value, target: target), at: insertionPoint!)
      }

      for (path, name) in program.ast.names(in: program.ast[pattern].subpattern) {
        let decl = program.ast[name].decl
        let declType = program.declTypes[decl]!

        locals[decl] = module.insert(
          BorrowInst(
            type: .address(declType),
            capability: capability,
            value: source,
            path: path,
            binding: decl,
            range: program.ast.ranges[decl]),
          at: insertionPoint!)
      }
    }
  }

  // MARK: Statements

  /// Emits the given statement into `module` at the current insertion point.
  private mutating func emit<T: StmtID>(stmt: T, into module: inout Module) {
    switch stmt.kind {
    case .braceStmt:
      emit(brace: NodeID(unsafeRawValue: stmt.rawValue), into: &module)
    case .declStmt:
      emit(declStmt: NodeID(unsafeRawValue: stmt.rawValue), into: &module)
    case .exprStmt:
      emit(exprStmt: NodeID(unsafeRawValue: stmt.rawValue), into: &module)
    case .returnStmt:
      emit(returnStmt: NodeID(unsafeRawValue: stmt.rawValue), into: &module)
    default:
      unreachable("unexpected statement")
    }
  }

  private mutating func emit(brace stmt: NodeID<BraceStmt>, into module: inout Module) {
    let localsBeforeBrace = locals
    for s in program.ast[stmt].stmts {
      emit(stmt: s, into: &module)
    }
    locals = localsBeforeBrace
  }

  private mutating func emit(declStmt stmt: NodeID<DeclStmt>, into module: inout Module) {
    switch program.ast[stmt].decl.kind {
    case .bindingDecl:
      emit(localBinding: NodeID(unsafeRawValue: program.ast[stmt].decl.rawValue), into: &module)
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

    module.insert(
      ReturnInst(value: value, range: program.ast.ranges[stmt]),
      at: insertionPoint!)
  }

  // MARK: r-values

  /// Emits `expr` as a r-value into `module` at the current insertion point.
  private mutating func emitR<T: ExprID>(expr: T, into module: inout Module) -> Operand {
    switch expr.kind {
    case .booleanLiteralExpr:
      return emitR(booleanLiteral: NodeID(unsafeRawValue: expr.rawValue), into: &module)
    case .condExpr:
      return emitR(cond: NodeID(unsafeRawValue: expr.rawValue), into: &module)
    case .funCallExpr:
      return emitR(funCall: NodeID(unsafeRawValue: expr.rawValue), into: &module)
    case .integerLiteralExpr:
      return emitR(integerLiteral: NodeID(unsafeRawValue: expr.rawValue), into: &module)
    case .nameExpr:
      return emitR(name: NodeID(unsafeRawValue: expr.rawValue), into: &module)
    case .sequenceExpr:
      return emitR(sequence: NodeID(unsafeRawValue: expr.rawValue), into: &module)
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
      RecordInst(objectType: .product(boolType), operands: [value]),
      at: insertionPoint!)
  }

  private mutating func emitR(
    cond expr: NodeID<CondExpr>,
    into module: inout Module
  ) -> Operand {
    let functionID = insertionPoint!.block.function

    // If the expression is supposed to return a value, allocate storage for it.
    var resultStorage: Operand?
    if let type = program.exprTypes[expr], type != .unit {
      resultStorage = module.insert(AllocStackInst(type), at: insertionPoint!)
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
        var condition = emitR(expr: itemExpr, into: &module)
        condition = module.insert(
          TakeMemberInst(type: .address(.builtin(.i(1))), value: condition, path: []),
          at: insertionPoint!)

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
      let value = emitR(expr: thenExpr, into: &module)
      if let target = resultStorage {
        let target = module.insert(
          BorrowInst(
            type: .address(program.exprTypes[expr]!), capability: .set, value: target, path: []),
          at: insertionPoint!)
        module.insert(StoreInst(object: value, target: target), at: insertionPoint!)
      }

    case .block:
      fatalError("not implemented")
    }
    module.insert(BranchInst(target: continuation), at: insertionPoint!)

    // Emit the failure branch.
    insertionPoint = InsertionPoint(endOf: alt!)
    switch program.ast[expr].failure {
    case .expr(let elseExpr):
      let value = emitR(expr: elseExpr, into: &module)
      if let target = resultStorage {
        let target = module.insert(
          BorrowInst(
            type: .address(program.exprTypes[expr]!), capability: .set, value: target, path: []),
          at: insertionPoint!)
        module.insert(StoreInst(object: value, target: target), at: insertionPoint!)
      }

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
        LoadInst(type: LoweredType(lowering: program.exprTypes[expr]!), source: source),
        at: insertionPoint!)
    } else {
      return .constant(.unit)
    }
  }

  private mutating func emitR(
    funCall expr: NodeID<FunCallExpr>,
    into module: inout Module
  ) -> Operand {
    guard case .lambda(let calleeType) = program.exprTypes[program.ast[expr].callee] else {
      unreachable()
    }

    // Determine the callee's convention.
    var conventions: [PassingConvention]
    switch calleeType.operatorProperty {
    case .inout:
      conventions = [.inout]
    case .sink:
      conventions = [.sink]
    default:
      conventions = [.let]
    }

    // Arguments are evaluated first, from left to right.
    var arguments: [Operand] = []

    for (parameter, argument) in zip(calleeType.inputs, program.ast[expr].arguments) {
      let parameterType = ParameterType(converting: parameter.type)!
      conventions.append(parameterType.convention)

      switch parameterType.convention {
      case .let:
        arguments.append(emitL(expr: argument.value.value, withCapability: .let, into: &module))
      case .inout:
        arguments.append(emitL(expr: argument.value.value, withCapability: .inout, into: &module))
      case .set:
        arguments.append(emitL(expr: argument.value.value, withCapability: .set, into: &module))
      case .sink:
        arguments.append(emitR(expr: argument.value.value, into: &module))
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
          name: program.ast[calleeID].stem.value,
          type: .address(.lambda(calleeType)))))

      case .direct(let calleeDeclID) where calleeDeclID.kind == .funDecl:
        // Callee is a direct reference to a function declaration.
        switch (program.ast[calleeDeclID] as! FunDecl).introducer.value {
        case .memberwiseInit:
          // The function is a memberwise initializer. In that case, the whole call expression is
          // lowered as a `record` instruction.
          return module.insert(
            RecordInst(objectType: program.exprTypes[expr]!, operands: arguments),
            at: insertionPoint!)

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
              BorrowInst(
                type: .address(type.base),
                capability: type.capability,
                value: locals[receiverDecl!]!,
                path: []),
              at: insertionPoint!)
            arguments.insert(receiver, at: 0)

          case .explicit(let receiverID):
            let receiver = emitL(expr: receiverID, withCapability: type.capability, into: &module)
            arguments.insert(receiver, at: 0)

          case .implicit:
            unreachable()
          }
        } else {
          // The receiver is consumed.
          conventions.insert(.sink, at: 1)

          switch program.ast[calleeID].domain {
          case .none:
            let receiver = module.insert(
              LoadInst(type: .object(receiverType), source: locals[receiverDecl!]!),
              at: insertionPoint!)
            arguments.insert(receiver, at: 0)

          case .explicit(let receiverID):
            arguments.insert(emitR(expr: receiverID, into: &module), at: 0)

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
        type: .object(program.exprTypes[expr]!),
        conventions: conventions,
        callee: callee,
        arguments: arguments),
      at: insertionPoint!)
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
        RecordInst(objectType: .product(type), operands: [.constant(.integer(value))]),
        at: insertionPoint!)

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
      if let source = locals[declID] {
        return module.insert(
          LoadInst(type: .object(program.exprTypes[expr]!), source: source),
          at: insertionPoint!)
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
        name: NodeID(unsafeRawValue: expr.rawValue), withCapability: capability, into: &module)

    case .subscriptCallExpr:
      fatalError("not implemented")

    default:
      let exprType = program.exprTypes[expr]!

      let value = emitR(expr: expr, into: &module)
      let storage = module.insert(AllocStackInst(exprType), at: insertionPoint!)

      let target = module.insert(
        BorrowInst(type: .address(exprType), capability: .set, value: storage, path: []),
        at: insertionPoint!)
      module.insert(StoreInst(object: value, target: target), at: insertionPoint!)

      return module.insert(
        BorrowInst(
          type: .address(exprType), capability: capability, value: storage, path: []),
        at: insertionPoint!)
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
      if let source = locals[declID] {
        return module.insert(
          BorrowInst(
            type: .address(program.exprTypes[expr]!),
            capability: capability,
            value: source,
            path: []),
          at: insertionPoint!)
      }

      fatalError("not implemented")

    case .member(let declID):
      // Emit the receiver.
      let receiver: Operand

      switch program.ast[expr].domain {
      case .none:
        receiver = locals[receiverDecl!]!
      case .implicit:
        fatalError("not implemented")
      case .explicit(let receiverID):
        receiver = emitL(expr: receiverID, withCapability: capability, into: &module)
      }

      // Emit the bound member.
      switch declID.kind {
      case .varDecl:
        let declID = NodeID<VarDecl>(unsafeRawValue: declID.rawValue)
        let layout = TypeLayout(module.type(of: receiver).astType, in: program)
        let memberIndex = layout.storedPropertiesIndices[program.ast[declID].name]!

        // If the lowered receiver is a borrow instruction, modify it in place so that it targets
        // the requested stored member. Otherwise, emit a reborrow.
        if let id = receiver.instID,
           let receiverInst = module[id.function][id.block][id.address] as? BorrowInst
        {
          module[id.function][id.block][id.address] = BorrowInst(
            type: .address(program.exprTypes[expr]!),
            capability: capability,
            value: receiverInst.value,
            path: receiverInst.path + [memberIndex])
          return receiver
        } else {
          let member = BorrowInst(
            type: .address(program.exprTypes[expr]!),
            capability: capability,
            value: receiver,
            path: [memberIndex])
          return module.insert(member, at: insertionPoint!)
        }

      default:
        fatalError("not implemented")
      }
    }

    fatalError()
  }

}
