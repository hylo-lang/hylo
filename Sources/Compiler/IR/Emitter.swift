import Utils

/// Val's IR emitter.
///
/// The emitter transforms well-formed, typed ASTs to a representation suitable for flow-sensitive
/// analysis and code generation.
public struct Emitter {

  /// The AST being lowered.
  public let ast: AST

  /// The scope hierarchy of the AST.
  public let scopeHierarchy: ScopeHierarchy

  /// The overarching type of each declaration.
  public let declTypes: DeclMap<Type>

  /// The type of each expression.
  public private(set) var exprTypes = ExprMap<Type>()

  /// A table mapping name expressions to referred declarations.
  public let referredDecls: [NodeID<NameExpr>: DeclRef]

  /// The insertion point of the emitter.
  public var insertionPoint: InsertionPoint?

  /// The local variables in scope.
  private var locals = DeclMap<Operand>()

  /// The declaration of the receiver of the function or subscript currently emitted, if any.
  private var receiverDecl: NodeID<ParameterDecl>?

  /// The memory region where local variables should be allocated.
  private var localMemorySpace = MemorySpace.stack

  public init(
    ast: AST,
    withScopeHierarchy scopeHierarchy: ScopeHierarchy,
    withDeclTypes declTypes: DeclMap<Type>,
    withExprTypes exprTypes: ExprMap<Type>,
    withReferredDecls referredDecls: [NodeID<NameExpr>: DeclRef]
  ) {
    self.ast = ast
    self.scopeHierarchy = scopeHierarchy
    self.declTypes = declTypes
    self.exprTypes = exprTypes
    self.referredDecls = referredDecls
  }

  public init(checker: TypeChecker) {
    self.init(
      ast: checker.ast,
      withScopeHierarchy: checker.scopeHierarchy,
      withDeclTypes: checker.declTypes,
      withExprTypes: checker.exprTypes,
      withReferredDecls: checker.referredDecls)
  }

  // MARK: Declarations

  /// Emits the Val IR of the module identified by `decl`.
  public mutating func emit(module decl: NodeID<ModuleDecl>) -> Module {
    var module = Module(decl: decl, id: ast[decl].name)
    for member in ast[decl].members {
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
      ast: ast,
      withScopeHierarchy: scopeHierarchy,
      withDeclTypes: declTypes)

    // Create the function entry.
    assert(module.functions[functionID].blocks.isEmpty)
    let entryID = module.createBasicBlock(
      accepting: module.functions[functionID].inputs,
      atEndOf: functionID)
    insertionPoint = InsertionPoint(endOf: entryID)

    // Configure the locals.
    var locals = DeclMap<Operand>()

    for (i, capture) in ast[declID].implicitParameterDecls.enumerated() {
      locals[capture.decl] = .parameter(block: entryID, index: i)
    }

    let implicitParamCount = ast[declID].implicitParameterDecls.count
    for (i, parameter) in ast[declID].parameters.enumerated() {
      locals[parameter] = .parameter(block: entryID, index: i + implicitParamCount)
    }

    // Emit the function's body.
    var receiverDecl = ast[declID].implicitReceiverDecl
    swap(&receiverDecl, &self.receiverDecl)
    swap(&locals, &self.locals)

    switch ast[declID].body!.value {
    case .block(let stmt):
      emit(stmt: stmt, into: &module)

    case .expr(let expr):
      let value = emitR(expr: expr, into: &module)
      if exprTypes[expr]! != .never {
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
    for member in ast[declID].members {
      // Emit the method and subscript members of the type declaration.
      switch member.kind {
      case .funDecl:
        let funDeclID = NodeID<FunDecl>(unsafeRawValue: member.rawValue)
        switch ast[funDeclID].introducer.value {
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
    default:
      unreachable("unexpected statement")
    }
  }

  private mutating func emit(brace stmt: NodeID<BraceStmt>, into module: inout Module) {
    let localsBeforeBrace = locals
    for s in ast[stmt].stmts {
      emit(stmt: s, into: &module)
    }
    locals = localsBeforeBrace
  }

  private mutating func emit(declStmt stmt: NodeID<DeclStmt>, into module: inout Module) {
    switch ast[stmt].decl.kind {
    case .bindingDecl:
      emit(localBinding: NodeID(unsafeRawValue: ast[stmt].decl.rawValue), into: &module)
    default:
      unreachable("unexpected declaration")
    }
  }

  private mutating func emit(exprStmt stmt: NodeID<ExprStmt>, into module: inout Module) {
    _ = emitR(expr: ast[stmt].expr, into: &module)
  }

  private mutating func emit(localBinding decl: NodeID<BindingDecl>, into module: inout Module) {
    let pattern = ast[decl].pattern
    switch ast[pattern].introducer.value {
    case .var, .sinklet:
      // Emit the initializer, if any.
      let initializer = ast[decl].initializer.map({ emitR(expr: $0, into: &module) })

      // Allocate storage for each name introduced by the declaration.
      for (path, name) in ast.names(in: ast[pattern].subpattern) {
        let decl = ast[name].decl
        let declType = declTypes[decl]!

        let alloc = module.insert(
          AllocInst(objectType: declType, space: localMemorySpace),
          at: insertionPoint!)
        locals[decl] = .inst(alloc)

        if let source = initializer {
          if path.isEmpty {
            _ = module.insert(
              StoreInst(object: source, target: .inst(alloc)),
              at: insertionPoint!)
          } else {
            let member = module.insert(
              MemberAddrInst(value: source, path: path, type: .address(declType)),
              at: insertionPoint!)
            let object = module.insert(
              LoadInst(source: .inst(member), type: .object(declType)),
              at: insertionPoint!)
            _ = module.insert(
              StoreInst(object: .inst(object), target: .inst(alloc)),
              at: insertionPoint!)
          }
        }
      }

    case .let, .inout:
      // There's nothing to do if there's no initializer.
      if let initializer = ast[decl].initializer {
        // Emit the initializer as a l-value if possible. Otherwise, emit a r-value and store it
        // into local storage.
        let source: Operand
        if (initializer.kind == .nameExpr) || (initializer.kind == .subscriptCallExpr) {
          source = emitL(expr: initializer, into: &module)
        } else {
          let value = emitR(expr: initializer, into: &module)
          let alloc = module.insert(
            AllocInst(objectType: exprTypes[initializer]!, space: localMemorySpace),
            at: insertionPoint!)
          _ = module.insert(
            StoreInst(object: value, target: .inst(alloc)),
            at: insertionPoint!)
          source = .inst(alloc)
        }

        for (path, name) in ast.names(in: ast[pattern].subpattern) {
          let decl = ast[name].decl
          let declType = declTypes[decl]!

          if path.isEmpty {
            locals[decl] = source
          } else {
            let member = module.insert(
              MemberAddrInst(value: source, path: path, type: .address(declType)),
              at: insertionPoint!)
            locals[decl] = .inst(member)
          }
        }
      }
    }
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
    let boolType = ProductType(standardLibraryTypeNamed: "Bool", ast: ast)!
    let boolValue = IntegerConstant(
      bitPattern: BitPattern(pattern: ast[expr].value ? 1 : 0, width: 1))

    let result = module.insert(
      RecordInst(type: .object(.product(boolType)), operands: [.constant(.integer(boolValue))]),
      at: insertionPoint!)
    return .inst(result)
  }

  private mutating func emitR(
    cond expr: NodeID<CondExpr>,
    into module: inout Module
  ) -> Operand {
    let functionID = insertionPoint!.block.function

    // If the expression is supposed to return a value, allocate storage for it.
    var resultStorage: InstID?
    if let type = exprTypes[expr], type != .unit {
      resultStorage = module.insert(
        AllocInst(objectType: type, space: localMemorySpace),
        at: insertionPoint!)
    }

    // Emit the condition(s).
    var alt: BlockID?

    for item in ast[expr].condition {
      let success = module.createBasicBlock(atEndOf: functionID)
      let failure = module.createBasicBlock(atEndOf: functionID)
      alt = failure

      switch item {
      case .expr(let itemExpr):
        // Evaluate the condition in the current block.
        var condition = emitL(expr: itemExpr, into: &module)
        condition = .inst(module.insert(
          MemberAddrInst(value: condition, path: [0], type: .address(.builtin(.i(1)))),
          at: insertionPoint!))
        condition = .inst(module.insert(
          LoadInst(source: condition, type: .object(.builtin(.i(1)))),
          at: insertionPoint!))

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
    switch ast[expr].success {
    case .expr(let thenExpr):
      let value = emitR(expr: thenExpr, into: &module)
      if let target = resultStorage {
        module.insert(StoreInst(object: value, target: .inst(target)), at: insertionPoint!)
      }

    case .block:
      fatalError("not implemented")
    }
    module.insert(BranchInst(target: continuation), at: insertionPoint!)

    // Emit the failure branch.
    insertionPoint = InsertionPoint(endOf: alt!)
    switch ast[expr].failure {
    case .expr(let elseExpr):
      let value = emitR(expr: elseExpr, into: &module)
      if let target = resultStorage {
        module.insert(StoreInst(object: value, target: .inst(target)), at: insertionPoint!)
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
      let load = module.insert(
        LoadInst(source: .inst(source), type: LoweredType(lowering: exprTypes[expr]!)),
        at: insertionPoint!)
      return .inst(load)
    } else {
      return .constant(.unit)
    }
  }

  private mutating func emitR(
    funCall expr: NodeID<FunCallExpr>,
    into module: inout Module
  ) -> Operand {
    guard case .lambda(let calleeType) = exprTypes[ast[expr].callee] else {
      unreachable()
    }

    // Emit the callee.
    let callee: Operand
    var operands: [Operand] = []

    if let calleeID = NodeID<NameExpr>(converting: ast[expr].callee) {
      // Attempt to interpret the callee as a direct function reference.
      switch referredDecls[calleeID] {
      case .direct(let calleeDeclID) where calleeDeclID.kind == .builtinDecl:
        // Callee refers to a built-in function.
        assert(calleeType.environment == .unit)
        callee = .constant(.builtin(BuiltinFunctionRef(
          name: ast[calleeID].stem.value,
          type: .address(.lambda(calleeType)))))

      case .direct(let calleeDeclID) where calleeDeclID.kind == .funDecl:
        // Callee is a direct reference to a function declaration.
        if (ast[calleeDeclID] as! FunDecl).introducer.value == .memberwiseInit {
          // Emit a record construction.
          let operands = ast[expr].arguments.map({ argument in
            emitR(expr: argument.value.value, into: &module)
          })
          let record = module.insert(
            RecordInst(type: .object(exprTypes[expr]!), operands: operands),
            at: insertionPoint!)
          return .inst(record)
        } else {
          // TODO: handle captures
          let locator = DeclLocator(
            identifying: calleeDeclID,
            in: ast,
            withScopeHierarchy: scopeHierarchy,
            withDeclTypes: declTypes)
          callee = .constant(.function(FunctionRef(
            name: locator.mangled,
            type: .address(.lambda(calleeType)))))
        }

      case .member(let calleeDeclID) where calleeDeclID.kind == .funDecl:
        // We may assume the callee refers to a method.
        guard let receiverType = calleeType.captures?[0].type else { unreachable() }

        // Add the receiver to the arguments.
        switch ast[calleeID].domain {
        case .none:
          operands.append(locals[receiverDecl!]!)
        case .implicit:
          fatalError("not implemented")
        case .explicit(let receiverID):
          if case .projection = receiverType {
            operands.append(emitL(expr: receiverID, into: &module))
          } else {
            operands.append(emitR(expr: receiverID, into: &module))
          }
        }

        // Emit the function reference.
        let locator = DeclLocator(
          identifying: calleeDeclID,
          in: ast,
          withScopeHierarchy: scopeHierarchy,
          withDeclTypes: declTypes)
        callee = .constant(.function(FunctionRef(
          name: locator.mangled,
          type: .address(.lambda(calleeType)))))

      default:
        callee = emitR(expr: ast[expr].callee, into: &module)
      }
    } else {
      // Interpret the callee as a closure expression.
      callee = emitR(expr: ast[expr].callee, into: &module)
    }

    for (parameter, argument) in zip(calleeType.inputs, ast[expr].arguments) {
      let parameterType = ParameterType(converting: parameter.type) ?? unreachable()
      switch parameterType.convention {
      case .let, .inout, .set:
        operands.append(emitL(expr: argument.value.value, into: &module))
      case .sink:
        operands.append(emitR(expr: argument.value.value, into: &module))
      case .yielded:
        unreachable()
      }
    }

    let i = module.insert(
      CallInst(callee: callee, operands: operands, type: .object(exprTypes[expr]!)),
      at: insertionPoint!)
    return .inst(i)
  }

  private mutating func emitR(
    integerLiteral expr: NodeID<IntegerLiteralExpr>,
    into module: inout Module
  ) -> Operand {
    guard case .product(let type) = exprTypes[expr]! else { unreachable() }

    switch type.name.value {
    case "Int":
      let bits = BitPattern(fromDecimal: ast[expr].value)!.resized(to: 64)
      let value = IntegerConstant(bitPattern: bits)
      let result = module.insert(
        RecordInst(type: .object(.product(type)), operands: [.constant(.integer(value))]),
        at: insertionPoint!)
      return .inst(result)

    default:
      unreachable("unexpected numeric type")
    }
  }

  private mutating func emitR(
    name expr: NodeID<NameExpr>,
    into module: inout Module
  ) -> Operand {
    let source = emitL(expr: expr, into: &module)
    let load = module.insert(
      LoadInst(source: source, type: .object(exprTypes[expr]!)),
      at: insertionPoint!)
    return .inst(load)
  }

  private mutating func emitR(
    sequence expr: NodeID<SequenceExpr>,
    into module: inout Module
  ) -> Operand {
    guard case .root(let root) = ast[expr] else { unreachable() }
    return emitR(expr: root, into: &module)
  }

  // MARK: l-values

  /// Emits `expr` as a l-value into `module` at the current insertion point.
  private mutating func emitL<T: ExprID>(expr: T, into module: inout Module) -> Operand {
    switch expr.kind {
    case .nameExpr:
      return emitL(name: NodeID(unsafeRawValue: expr.rawValue), into: &module)

    default:
      let value = emitR(expr: expr, into: &module)
      let alloc = module.insert(
        AllocInst(objectType: exprTypes[expr]!, space: localMemorySpace),
        at: insertionPoint!)
      _ = module.insert(
        StoreInst(object: value, target: .inst(alloc)),
        at: insertionPoint!)
      return .inst(alloc)
    }
  }

  private mutating func emitL(
    name expr: NodeID<NameExpr>,
    into module: inout Module
  ) -> Operand {
    let exprType = exprTypes[expr]!
    guard let declRef = referredDecls[expr] else {
      return .constant(.poison(PoisonConstant(type: .address(exprType))))
    }

    switch declRef {
    case .direct(let declID):
      // Lookup for a local symbol.
      if let local = locals[declID] {
        return local
      }

      fatalError("not implemented")

    case .member(let declID):
      // Emit the receiver.
      let receiver: Operand

      switch ast[expr].domain {
      case .none:
        receiver = locals[receiverDecl!]!
      case .implicit:
        fatalError("not implemented")
      case .explicit(let receiverID):
        receiver = emitL(expr: receiverID, into: &module)
      }

      // Emit the bound member.
      switch declID.kind {
      case .varDecl:
        let layout = TypeLayout(module.type(of: receiver).astType)
        let member = module.insert(
          MemberAddrInst(
            value: receiver,
            path: [layout.offset(of: NodeID(unsafeRawValue: declID.rawValue), ast: ast)],
            type: .address(exprType)),
          at: insertionPoint!)
        return .inst(member)

      default:
        fatalError("not implemented")
      }
    }

    fatalError()
  }

}
