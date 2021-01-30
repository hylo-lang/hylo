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

  /// Resolves an `UnresolvedDeclRefExpr`, performing an unqualified name lookup from the
  /// declaration space in which it resides.
  func visit(_ node: UnresolvedDeclRefExpr) -> Expr {
    let context = node.type.context

    let matches = space.lookup(unqualified: node.name, in: node.type.context)
    guard !matches.isEmpty else {
      context.report(.cannotFind(symbol: node.name, range: node.range))
      return ErrorExpr(type: context.errorType, range: node.range)
    }

    return bind(ref: node, to: matches)
  }

  /// Resolves an `UnresolvedQualDeclRefExpr`, performing a qualified name lookup on its base.
  func visit(_ node: UnresolvedQualDeclRefExpr) -> Expr {
    let context = node.type.context

    let baseType = node.namespace.realize(unqualifiedFrom: space)
    guard !(baseType is ErrorType) else {
      // The diagnostic is emitted by the failed attempt to realize the base.
      return ErrorExpr(type: context.errorType, range: node.range)
    }

    // Handle built-ins.
    if baseType === context.builtin.instanceType {
      guard let decl = context.getBuiltinDecl(for: node.name) else {
        context.report(.cannotFind(builtin: node.name, range: node.range))
        return ErrorExpr(type: context.errorType, range: node.range)
      }

      // There's no need to instantiate the type, built-ins are never generic.
      return DeclRefExpr(decl: decl, type: decl.realize(), range: node.range)
    }

    // Run a qualified lookup if the namespace resolved to a nominal type.
    let matches = baseType.lookup(member: node.name)
    guard !matches.isEmpty else {
      context.report(.cannotFind(member: node.name, in: baseType, range: node.range))
      return ErrorExpr(type: context.errorType, range: node.range)
    }

    // Note that this will throw the type repr away.
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

  func visit(_ node: UnresolvedMemberExpr) -> Expr {
    let context = node.type.context

    // Propagate error expressions.
    if node.base is ErrorExpr {
      // We can assume a diagnostic has been emitted by a previous attempt to bind the base. We
      // need a superfluous "error type has no member ..." diagnostic.
      return ErrorExpr(type: context.errorType, range: node.range)
    }

    // If the base is known to resolve to a concrete type, we could attempt to resolve the whole
    // expression as a `MemberRefExpr` directly here, rather than letting the type solver do it.
    // However, this wouldn't for overloaded members, unless we modify `OverloadedDeclRefExpr` so
    // that it can keep track of a base expression.

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

  func visit(_ node: ErrorExpr) -> Expr {
    return node
  }

  /// Desugars a `CallExpr` to a constructor (e.g., `Foo(bar: 1)`).
  ///
  /// - Parameter call: A call expression whose `fun` is a `TypeDeclRefExpr`.
  func desugar(constructorCall call: CallExpr) -> CallExpr {
    let context = call.type.context

    func newCall(newFun: Expr) -> CallExpr {
      return CallExpr(fun: newFun, args: call.args, type: call.type, range: call.range)
    }

    let fun = call.fun as! TypeDeclRefExpr
    guard let typeDecl = fun.decl as? AbstractNominalTypeDecl else {
      context.report(.cannotFind(member: "new", in: fun.decl.type, range: fun.range))
      return newCall(newFun: ErrorExpr(type: context.errorType, range: fun.range))
    }

    let matches = typeDecl.lookup(qualified: "new")
    guard !matches.values.isEmpty else {
      context.report(.cannotFind(member: "new", in: fun.decl.type, range: fun.range))
      return newCall(newFun: ErrorExpr(type: context.errorType, range: fun.range))
    }

    let newFun = bind(ref: fun, to: matches)
    assert(!(newFun is TypeDeclRefExpr))
    return newCall(newFun: newFun)
  }

  /// Binds the given declaration reference to specified lookup result.
  func bind(ref: Expr, to matches: LookupResult) -> Expr {
    let context = ref.type.context

    // Favor references to value declarations.
    if matches.values.count > 1 {
      let newRef = OverloadedDeclRefExpr(
        subExpr: ref, declSet: matches.values, type: ref.type, range: ref.range)
      newRef.type = ref.type is TypeVar
        ? ref.type
        : TypeVar(context: context, node: newRef)

      return newRef
    }

    if let decl = matches.values.first {
      // Instanciate the declaration's type if it's generic.
      let instType = decl.instantiate(from: space)

      // If `ref` is a member expression, make sure we keep its base around.
      if let expr = ref as? MemberExpr {
        // Preserve the base expr.
        return MemberRefExpr(base: expr.base, decl: decl, type: instType, range: expr.range)
      } else {
        return DeclRefExpr(decl: decl, type: instType, range: ref.range)
      }
    }

    // FIXME: Handle overloaded type decls.
    precondition(matches.count == 1, "overloaded type declarations are not supported yet")
    return TypeDeclRefExpr(decl: matches.types[0], range: ref.range)
  }

}
