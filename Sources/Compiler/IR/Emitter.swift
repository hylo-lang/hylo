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
    var module = Module(id: ast[decl].name)
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
    case .productTypeDecl:
      emit(product: NodeID(unsafeRawValue: decl.rawValue), into: &module)
    default:
      unreachable("unexpected declaration")
    }
  }

  /// Emits the given function declaration into `module`.
  public mutating func emit(fun declID: NodeID<FunDecl>, into module: inout Module) {
    switch ast[declID].introducer.value {
    case .memberwiseInit, .`init`, .deinit:
      // TODO
      return

    case .fun:
      break
    }

    precondition(ast[declID].body != nil, "no function body")

    switch declTypes[declID] {
    case .lambda(let type):
      // Declare the function in the module if necessary.
      let functionID = module.getOrCreateFunction(
        from: declID,
        ast: ast,
        withScopeHierarchy: scopeHierarchy,
        withDeclTypes: declTypes)

      // Create the function entry.
      assert(module.functions[functionID].blocks.isEmpty)
      var inputs: [IRType] = []
      inputs.reserveCapacity(type.captures!.count + type.inputs.count)

      for capture in ast[declID].implicitParameterDecls {
        inputs.append(IRType(declTypes[capture.decl]!))
      }

      for parameter in ast[declID].parameters {
        inputs.append(IRType(declTypes[parameter]!))
      }

      let entryIndex = module.functions[functionID].blocks.append(Block(
        inputs: inputs,
        instructions: []))
      let entryID = BlockID(function: functionID, index: entryIndex)
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
        module.insert(ReturnInst(value: value), at: insertionPoint!)

      case .bundle:
        unreachable()
      }

      swap(&locals, &self.locals)
      swap(&receiverDecl, &self.receiverDecl)

    case .method:
      fatalError("not implemented")

    default:
      unreachable()
    }
  }

  /// Emits the product type declaration into `module`.
  private mutating func emit(product declID: NodeID<ProductTypeDecl>, into module: inout Module) {
    for member in ast[declID].members {
      // Emit the method and subscript members of the type declaration.
      switch member.kind {
      case .funDecl:
        emit(fun: NodeID(unsafeRawValue: member.rawValue), into: &module)
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
    fatalError("not implemented")
  }

  // MARK: r-values

  /// Emits `expr` as a r-value into `module` at the current insertion point.
  private mutating func emitR<T: ExprID>(expr: T, into module: inout Module) -> Operand {
    switch expr.kind {
    case .funCallExpr:
      return emitR(funCall: NodeID(unsafeRawValue: expr.rawValue), into: &module)
    case .nameExpr:
      return emitR(name: NodeID(unsafeRawValue: expr.rawValue), into: &module)
    default:
      unreachable("unexpected expression")
    }
  }

  private mutating func emitR(
    funCall expr: NodeID<FunCallExpr>,
    into module: inout Module
  ) -> Operand {
    guard case .lambda(let calleeType) = exprTypes[ast[expr].callee] else {
      unreachable()
    }

    // If `callee` denotes a name expression, attempts to emit a function reference.
    let callee: Operand
    if ast[expr].callee.kind == .nameExpr {
      let nameExprID = NodeID<NameExpr>(unsafeRawValue: ast[expr].callee.rawValue)

      switch referredDecls[nameExprID] {
      case .direct(let declID):
        switch declID.kind {
        case .builtinDecl:
          // Built-in functions never have closures.
          assert(calleeType.environment == .unit)
          callee = .constant(.builtin(BuiltinFunctionRef(
            name: ast[nameExprID].stem.value,
            type: .address(.lambda(calleeType)))))

        case .funDecl:
          switch (ast[declID] as! FunDecl).introducer.value {
          case .memberwiseInit:
            // Emit a record construction.
            let operands = ast[expr].arguments.map({ argument in
              emitR(expr: argument.value.value, into: &module)
            })
            let record = module.insert(
              RecordInst(type: .owned(exprTypes[expr]!), operands: operands),
              at: insertionPoint!)
            return .inst(record)

          default:
            // TODO: handle captures
            let locator = DeclLocator(
              identifying: declID,
              in: ast,
              withScopeHierarchy: scopeHierarchy,
              withDeclTypes: declTypes)
            callee = .constant(.function(FunctionRef(
              name: locator.mangled,
              type: .address(.lambda(calleeType)))))
          }

        default:
          unreachable("unexpected declaration")
        }

      case .member:
        fatalError("not implemented")

      case nil:
        fatalError("unbound name expression")
      }
    } else {
      // The callee denotes the expression of a closure.
      callee = emitR(expr: ast[expr].callee, into: &module)
    }

    var arguments: [Operand] = []
    for (parameter, argument) in zip(calleeType.inputs, ast[expr].arguments) {
      let parameterType = ParameterType(converting: parameter.type) ?? unreachable()
      switch parameterType.convention {
      case .let, .inout, .set:
        arguments.append(emitL(expr: argument.value.value, into: &module))
      case .sink:
        arguments.append(emitR(expr: argument.value.value, into: &module))
      case .yielded:
        unreachable()
      }
    }

    let i = module.insert(
      CallInst(callee: callee, arguments: arguments, type: .owned(exprTypes[expr]!)),
      at: insertionPoint!)
    return .inst(i)
  }

  private mutating func emitR(
    name expr: NodeID<NameExpr>,
    into module: inout Module
  ) -> Operand {
    let source = emitL(expr: expr, into: &module)
    let load = module.insert(
      LoadInst(source: source, type: .owned(exprTypes[expr]!)),
      at: insertionPoint!)
    return .inst(load)
  }

  // MARK: l-values

  /// Emits `expr` as a l-value into `module` at the current insertion point.
  private mutating func emitL<T: ExprID>(expr: T, into module: inout Module) -> Operand {
    switch expr.kind {
    case .nameExpr:
      return emitL(name: NodeID(unsafeRawValue: expr.rawValue), into: &module)
    default:
      unreachable("unexpected expression")
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
    case .direct:
      fatalError("not implemented")

    case .member(let declID):
      // References to bound member declaration create an access to `self`.
      let receiver = locals[receiverDecl!]!

      switch declID.kind {
      case .varDecl:
        let layout = TypeLayout(module.type(of: receiver).valType)
        let member = module.insert(
          MemberAddrInst(
            value: receiver,
            offset: layout.offset(of: NodeID(unsafeRawValue: declID.rawValue), ast: ast),
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
