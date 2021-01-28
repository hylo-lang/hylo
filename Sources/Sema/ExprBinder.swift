import AST

struct ExprBinder: ExprVisitor {

  typealias ExprResult = Expr

  /// The declaration space in which the visited expression resides.
  var space: DeclSpace

  func visit(_ node: IntLiteralExpr) -> Expr {
    return node
  }

  func visit(_ node: AssignExpr) -> Expr {
    return node
  }

  func visit(_ node: CallExpr) -> Expr {
    if node.fun is TypeDeclRefExpr {
      return desugar(constructorCall: node)
    }
    return node
  }

  /// Resolves an `UnresolvedDeclRefExpr`, performing an unqualified name lookup.
  ///
  /// - Parameter node: The expression to resolve.
  func visit(_ node: UnresolvedDeclRefExpr) -> Expr {
    let context = node.type.context

    let matches = space.lookup(unqualified: node.name, in: node.type.context)
    guard !matches.isEmpty else {
      context.report(.cannotFind(symbol: node.name, range: node.range))
      return node
    }

    return bind(ref: node, to: matches)
  }

  func visit(_ node: UnresolvedMemberExpr) -> Expr {
    return node
  }

  /// Resolves a `QualDeclRefExpr`, performing a qualified name lookup on its base.
  ///
  /// - Parameter ref: The expression to resolve.
  func visit(_ node: QualDeclRefExpr) -> Expr {
    let context = node.type.context

    guard let baseType = node.namespace.realize(within: space) else {
      // The diagnostic is emitted by the failed attempt to realize the base.
      return node
    }

    // Handle built-ins.
    if baseType === context.builtin.instanceType {
      guard let decl = context.getBuiltinDecl(for: node.name) else {
        context.report(.cannotFind(builtin: node.name, range: node.range))
        return node
      }
      return DeclRefExpr(decl: decl, range: node.range)
    }

    // Run a qualified lookup if the namespace resolved to a nominal type.
    guard let baseTypeDecl = (baseType as? NominalType)?.decl else {
      context.report(.cannotFind(member: node.name, in: baseType, range: node.range))
      return node
    }
    let matches = baseTypeDecl.lookup(qualified: node.name)
    guard !matches.isEmpty else {
      context.report(.cannotFind(member: node.name, in: baseType, range: node.range))
      return node
    }

    return bind(ref: node, to: matches)
  }

  func visit(_ node: OverloadedDeclRefExpr) -> Expr {
    return node
  }

  func visit(_ node: DeclRefExpr) -> Expr {
    return node
  }

  func visit(_ node: TypeDeclRefExpr) -> Expr {
    return node
  }

  func visit(_ node: MemberRefExpr) -> Expr {
    return node
  }

  func visit(_ node: AddrOfExpr) -> Expr {
    return node
  }

  func visit(_ node: WildcardExpr) -> Expr {
    return node
  }

  /// Desugars a `CallExpr` to a constructor (e.g., `Foo(bar: 1)`).
  ///
  /// - Parameter call: A call expression whose `fun` is a `TypeDeclRefExpr`.
  func desugar(constructorCall call: CallExpr) -> CallExpr {
    let context = call.type.context

    let fun = call.fun as! TypeDeclRefExpr
    guard let typeDecl = fun.decl as? AbstractNominalTypeDecl else {
      context.report(.cannotFind(member: "new", in: fun.decl.type, range: fun.range))
      return call
    }

    let matches = typeDecl.lookup(qualified: "new")
    guard !matches.values.isEmpty else {
      context.report(.cannotFind(member: "new", in: fun.decl.type, range: fun.range))
      return call
    }

    let newFun = bind(ref: fun, to: matches)
    assert(!(newFun is TypeDeclRefExpr))
    return CallExpr(fun: newFun, args: call.args, type: call.type, range: call.range)
  }

  /// Binds the given declaration reference to specified lookup result.
  func bind(ref: Expr, to matches: LookupResult) -> Expr {
    let context = ref.type.context

    // Favor references to value declarations.
    if matches.values.count > 1 {
      let newRef = OverloadedDeclRefExpr(
        declSet: matches.values, type: context.unresolvedType, range: ref.range)
      newRef.type = TypeVar(context: context, node: newRef)
      return newRef
    }

    if matches.values.count == 1 {
      return DeclRefExpr(decl: matches.values[0], range: ref.range)
    }

    // FIXME: Handle overloaded type decls.
    precondition(matches.count == 1, "overloaded type declarations are not supported yet")
    return TypeDeclRefExpr(decl: matches.types[0], range: ref.range)
  }

}
