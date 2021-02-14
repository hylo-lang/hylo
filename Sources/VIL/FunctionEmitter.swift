import AST

/// A visitor that emits the VIL code of a function declaration.
final class FunctionEmitter: StmtVisitor, ExprVisitor {

  typealias StmtResult = Void
  typealias ExprResult = Result<Value, EmitterError>

  /// The top-level emitter.
  unowned let parent: Emitter

  /// The declaration of the function being emitted.
  let funDecl: BaseFunDecl

  /// The VIL builder used by the emitter.
  var builder: Builder { parent.builder }

  /// The context in which the function declaration is defined.
  var context: AST.Context { funDecl.type.context }

  /// A symbol table that locally visible declarations to their emitted value.
  ///
  /// This is populated by function parameters and pattern binding declarations.
  var locals: [ObjectIdentifier: Value] = [:]

  /// Creates a function emitter.
  ///
  /// - Parameters:
  ///   - parent: The top-level emitter.
  ///   - builder: The builder used to create new instructions.
  ///   - funDecl: The declaration of the function to emit. The initializer will fail if `funDecl`
  ///   is not type checked.
  init(parent: Emitter, funDecl: BaseFunDecl) {
    precondition(funDecl.state >= .typeChecked)
    self.parent = parent
    self.funDecl = funDecl
  }

  /// Emits the function.
  func emit() {
    // Create (i.e., declare) the function in the module.
    var mangler = Mangler()
    mangler.append(funDecl: funDecl)
    let name = mangler.finalize()
    let function = builder.getOrCreateFunction(name: name, type: funDecl.unappliedType as! FunType)

    // We're done if the function doesn't have body.
    guard let body = funDecl.body else { return }

    // Create an entry block.
    builder.block = function.createBasicBlock(arguments: function.arguments)
    var arguments = function.arguments[0...]

    // Register the function's receiver in the local symbol table, if necessary.
    if funDecl.isMember {
      // Member functions accept their receiver has their first parameter.
      locals[ObjectIdentifier(funDecl.selfDecl!)] = function.arguments[0]
      arguments = arguments.dropFirst()
    } else if (funDecl is CtorDecl) {
      // Constructors should allocate `self`.
      let selfType = (funDecl.selfDecl!.type as! InoutType).base
      locals[ObjectIdentifier(funDecl.selfDecl!)] = builder.buildAllocStack(type: selfType)
    }

    // Register the function's formal parameters in the local symbol table..
    assert(funDecl.params.count == arguments.count)
    for (param, argument) in zip(funDecl.params, arguments) {
      locals[ObjectIdentifier(param)] = argument
    }

    // Emit the function's body.
    visit(body)

    // If the function's a constructor, emit the implicit return statement.
    if funDecl is CtorDecl {
      let selfLoc = locals[ObjectIdentifier(funDecl.selfDecl!)]
      let selfVal = builder.buildLoad(lvalue: selfLoc!)
      builder.buildRet(value: selfVal)
    }
  }

  func emit(localPBDecl node: PatternBindingDecl) {
    // Create the variable locations for each name in the pattern.
    let lvs = node.pattern.namedPatterns.map({ name in
      emit(localVarDecl: name.decl, type: name.type)
    })

    // Emit the initializer, if any.
    if let initializer = node.initializer {
      let rv = emit(expr: initializer)

      // Emit a store right away if the pattern matches a single value.
      if node.pattern.singleVarDecl != nil {
        assert(lvs.count == 1)
        builder.buildStore(lvalue: lvs[0], rvalue: rv)
      } else {
        // FIXME: Handle destructuring,
        fatalError()
      }
    }
  }

  func emit(localVarDecl node: VarDecl, type: ValType) -> Value {
    guard node.state >= .typeChecked else {
      return ErrorValue(context: context)
    }
    precondition(node.hasStorage, "computed properties are not supported yet")

    // Allocate storage on the stack for the variable.
    let value = builder.buildAllocStack(type: node.type)
    locals[ObjectIdentifier(node)] = value
    return value
  }

  func emit(expr node: Expr) -> Value {
    // Emit an error value for any expression that has an error type.
    guard node.type !== context.errorType else {
      return ErrorValue(context: context)
    }

    switch node.accept(self) {
    case .success(let value):
      return value

    case .failure(let error):
      print(error)
      return ErrorValue(context: context)
    }
  }

  func visit(_ node: BraceStmt) {
    precondition(builder.block != nil, "not in a basic block")

    for i in 0 ..< node.stmts.count {
      switch node.stmts[i] {
      case let pdDecl as PatternBindingDecl:
        emit(localPBDecl: pdDecl)
        
      case let decl as Decl:
        parent.emit(decl: decl)

      case let stmt as Stmt:
        stmt.accept(self)

        // Discard everything after a return statement.
        if (stmt is RetStmt) && (i > node.stmts.count - 1) {
          context.report(.codeAfterReturnNeverExecuted(range: node.stmts[i + 1].range))
          break
        }

      case let expr as Expr:
        _ = expr.accept(self)

      default:
        fatalError("unreachable")
      }
    }
  }

  func visit(_ node: RetStmt) {
  }

  func visit(_ node: IntLiteralExpr) -> ExprResult {
    // We can assume the expression's type conforms to `ExpressibleBy***Literal` (as type checking
    // succeeded). Therefore we can look for a conversion constructor `new(literal:)`.

    // FIXME: There must be a more reliable way to retrieve the constructor declaration.
    let ctorType = context.funType(
      paramType: context.tupleType([
        TupleType.Elem(label: "literal", type: context.getBuiltinType(named: "IntLiteral")!)
      ]),
      retType: node.type)
    let ctorDecl = node.type
      .lookup(member: "new").values
      .first(where: { $0.type === ctorType }) as! CtorDecl

    // Get the VIL function corresponding to the conversion constructor.
    var mangler = Mangler()
    mangler.append(funDecl: ctorDecl)
    let function = builder.getOrCreateFunction(
      name: mangler.finalize(), type: ctorDecl.unappliedType as! FunType)

    // Emit a call to the constructor.
    let funref = FunRef(function: function)
    let literal = IntLiteralValue(value: node.value, context: context)
    return .success(builder.buildApply(fun: funref, args: [literal]))
  }

  func visit(_ node: AssignExpr) -> ExprResult {
    // Emit the left operand first.
    switch node.lvalue.accept(LValueEmitter(parent: self)) {
    case .success(let lvalue):
      let rvalue = emit(expr: node.rvalue)
      builder.buildStore(lvalue: lvalue, rvalue: rvalue)

    case .failure(let error):
      // Diagnostic common l-value errors.
      switch error {
      case .immutableSelf:
        context.report(.cannotAssignImmutableSelf(range: node.lvalue.range))
      case .immutableLocation:
        context.report(.cannotAssignToImmutableLocation(range: node.lvalue.range))
      }

      return .success(UnitValue(context: context))
    }

    // Assignments always result in a unit value.
    return .success(UnitValue(context: context))
  }

  func visit(_ node: TupleExpr) -> ExprResult {
    let tuple = builder.buildTuple(
      type: node.type as! TupleType,
      elems: node.elems.map({ elem in emit(expr: elem.value) }))
    return .success(tuple)
  }

  func visit(_ node: CallExpr) -> ExprResult {
    // Emit the callee and its arguments.
    let funref = emit(expr: node.fun)
    let args = node.args.map({ (arg: TupleElem) -> Value in
      return emit(expr: arg.value)
    })

    return .success(builder.buildApply(fun: funref, args: args))
  }

  func visit(_ node: UnresolvedDeclRefExpr) -> ExprResult {
    fatalError()
  }

  func visit(_ node: UnresolvedMemberExpr) -> ExprResult {
    fatalError()
  }

  func visit(_ node: UnresolvedQualDeclRefExpr) -> ExprResult {
    fatalError()
  }

  func visit(_ node: OverloadedDeclRefExpr) -> ExprResult {
    fatalError()
  }

  func visit(_ node: DeclRefExpr) -> ExprResult {
    // FIXME: We need a better, more reliable way to easily determine whether the node requires
    // l-value to r-value conversion.
    if let decl = node.decl as? FunDecl {
      if decl.props.contains(.isBuiltin) {
        return .success(BuiltinFunRef(decl: decl))
      }
    }

    if let decl = node.decl as? FunParamDecl, !(decl.type is InoutType) {
      let rv = locals[ObjectIdentifier(decl)]!
      return .success(rv)
    }

    // Emit a l-value and convert it to an r-value.
    switch node.accept(LValueEmitter(parent: self)) {
    case .success(let lvalue):
      return .success(builder.buildLoad(lvalue: lvalue))
    case let failure:
      return failure
    }
  }

  func visit(_ node: TypeDeclRefExpr) -> ExprResult {
    fatalError()
  }

  func visit(_ node: MemberRefExpr) -> ExprResult {
    // Emit the base value.
    let base: Value
    switch node.base.accept(self) {
    case .success(let b):
      base = b
    case let failure:
      return failure
    }

    switch node.base.type {
    case is ProductType:
      if let decl = node.decl as? VarDecl {
        if decl.hasStorage {
          return .success(builder.buildRecordMember(record: base, memberDecl: decl))
        }
      }

    default:
      break
    }

    fatalError()
  }

  func visit(_ node: AddrOfExpr) -> ExprResult {
    fatalError()
  }

  func visit(_ node: WildcardExpr) -> ExprResult {
    fatalError()
  }

  func visit(_ node: ErrorExpr) -> ExprResult {
    fatalError()
  }

}
