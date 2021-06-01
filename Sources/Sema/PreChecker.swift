import AST

/// A driver for a pre-check visitor.
final class PreCheckDriver: NodeWalker {

  init(system: UnsafeMutablePointer<ConstraintSystem>, useSite: DeclSpace) {
    self.system = system
    super.init(innermostSpace: useSite)
  }

  /// A pointer to the system in which new constraints are inserted.
  let system: UnsafeMutablePointer<ConstraintSystem>

  override func willVisit(_ expr: Expr) -> (shouldWalk: Bool, nodeBefore: Expr) {
    switch expr {
    case let tupleExpr as TupleExpr:
      // Substitute 'e' for '(e)', effectively eliminating parenthesized expressions.
      if (tupleExpr.elems.count == 1) && (tupleExpr.elems[0].label == nil) {
        return (true, tupleExpr.elems[0].value)
      } else {
        return (true, tupleExpr)
      }

    case let matchExpr as MatchExpr:
      // Match expressions require special handling to deal with case inference.
      let newExpr = matchExpr.accept(PreChecker(system: system, useSite: innermostSpace!))
      return (false, newExpr)

    case is ErrorExpr:
      return (false, expr)

    default:
      return (true, expr)
    }
  }

  override func didVisit(_ expr: Expr) -> (shouldContinue: Bool, nodeAfter: Expr) {
    let newExpr = expr.accept(
      PreChecker(system: system, useSite: innermostSpace!))
    return (true, newExpr)
  }

}

/// A pre-check visitor, that resolves primary names, realizes type representations and desugars
/// various expressions.
struct PreChecker: ExprVisitor {

  typealias ExprResult = Expr

  /// A pointer to the system in which new constraints are inserted.
  let system: UnsafeMutablePointer<ConstraintSystem>

  /// The declaration space in which the visited expression resides.
  let useSite: DeclSpace

  func visit(_ node: IntLiteralExpr) -> Expr {
    return node
  }

  func visit(_ node: AssignExpr) -> Expr {
    return node
  }

  func visit(_ node: BaseCastExpr) -> Expr {
    return node.accept(self)
  }

  func visit(_ node: DynCastExpr) -> Expr {
    let context = node.type.context
    let maybeDecl = context.getTypeDecl(for: .Maybe) as! AliasTypeDecl
    let targetType = node.sign.realize(unqualifiedFrom: useSite)

    node.type = context.boundGenericType(decl: maybeDecl, args: [targetType])
    return node
  }

  func visit(_ node: UnsafeCastExpr) -> Expr {
    node.type = node.sign.realize(unqualifiedFrom: useSite)
    return node
  }

  func visit(_ node: TupleExpr) -> Expr {
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

    let matches = useSite.lookup(unqualified: node.name, in: node.type.context)
    guard !matches.isEmpty else {
      context.report(.cannotFind(symbol: node.name, range: node.range))
      return ErrorExpr(type: context.errorType, range: node.range)
    }

    return bind(ref: node, to: matches)
  }

  /// Resolves an `UnresolvedQualDeclRefExpr`, performing a qualified name lookup on its base.
  func visit(_ node: UnresolvedQualDeclRefExpr) -> Expr {
    let context = node.type.context

    let baseType = node.namespace.realize(unqualifiedFrom: useSite)
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

      // There's no need to contextualize the type, built-ins are never generic.
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
    // expression as a `MemberDeclRefExpr` directly here, rather than having the type solver do it.
    // However, this won't work for overloaded members, unless we modify `OverloadedDeclRefExpr` so
    // that it can keep track of a base expression.
    return node
  }

  func visit(_ node: MemberDeclRefExpr) -> Expr {
    return node
  }

  func visit(_ node: TupleMemberExpr) -> Expr {
    let context = node.type.context

    // Propagate error expressions.
    if node.base is ErrorExpr {
      // We can assume a diagnostic has been emitted by a previous attempt to bind the base.
      return ErrorExpr(type: context.errorType, range: node.range)
    }

    return node
  }

  func visit(_ node: AsyncExpr) -> Expr {
    return node
  }

  func visit(_ node: AwaitExpr) -> Expr {
    return node
  }

  func visit(_ node: AddrOfExpr) -> Expr {
    return node
  }

  func visit(_ node: MatchExpr) -> Expr {
    // Type check the subject of the match before visiting its cases, so that its type can be used
    // to infer that of each case's pattern. This is done in a separate constraint system, since
    // the subject's type does't depend on the expression in which the match appears (only the type
    // of the match itself does). Hence, since case patterns do not contribute to the inference of
    // the subject's type they cannot help disambiguate overloading.
    TypeChecker.check(expr: &node.subject, useSite: useSite)

    // Bail out if the subjet doesn't have a valid type.
    let context = node.type.context
    guard !node.subject.type.hasErrors else {
      return ErrorExpr(type: context.errorType, range: node.range)
    }

    // Type check each case statement.
    for i in 0 ..< node.cases.count {
      var cs = ConstraintSystem()
      TypeChecker.check(
        pattern     : node.cases[i].pattern,
        expectedType: node.subject.type,
        useSite     : useSite,
        system      : &cs)
      TypeChecker.check(stmt: node.cases[i].body, useSite: useSite)
    }

    // If the match is not a sub-expression, we can just type it as `Unit`.
    guard node.isSubExpr else {
      node.type = context.unitType
      return node
    }

    // If the match is a sub-expression, make sure that its cases are single expressions and keep
    // its type unresolved.
    for stmt in node.cases {
      guard (stmt.body.stmts.count == 1) && (stmt.body.stmts[0] is Expr) else {
        context.report(.multipleStatementInMatchExpression(range: stmt.range))
        return ErrorExpr(type: context.errorType, range: node.range)
      }
    }

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
    func newCall(newFunExpr: Expr) -> CallExpr {
      return CallExpr(fun: newFunExpr, args: call.args, type: call.type, range: call.range)
    }

    let context = call.type.context

    // Search for a constructor declaration
    let matches = (call.fun as! TypeDeclRefExpr).decl.instanceType.lookup(member: "new")
    guard !matches.values.isEmpty else {
      context.report(.cannotFind(member: "new", in: call.fun.type, range: call.fun.range))
      return newCall(newFunExpr: ErrorExpr(type: context.errorType, range: call.fun.range))
    }

    // Substitute the callee for an overload set.
    let newFunExpr = bind(ref: call.fun, to: matches)
    assert(!(newFunExpr is TypeDeclRefExpr))

    // Desugar the constructor call.
    return newCall(newFunExpr: newFunExpr)
  }

  /// Binds the given declaration reference to specified lookup result.
  func bind(ref: Expr, to matches: LookupResult) -> Expr {
    // Favor references to value declarations.
    if matches.values.count > 1 {
      assert(!ref.type.hasVariables)
      let unresolved = ref.type.context.unresolvedType
      return OverloadedDeclRefExpr(
        subExpr: ref, declSet: matches.values, type: unresolved, range: ref.range)
    }

    if let decl = matches.values.first {
      // If `ref` is a member expression, make sure we keep its base around.
      let newRef: Expr
      if let expr = ref as? MemberExpr {
        // Preserve the base expr.
        newRef = MemberDeclRefExpr(base: expr.base, decl: decl, type: ref.type, range: expr.range)
      } else {
        newRef = DeclRefExpr(decl: decl, type: ref.type, range: ref.range)
      }

      // Contextualize the declaration's type if it's generic.
      newRef.type = TypeChecker.contextualize(
        decl: decl,
        from: useSite,
        processingContraintsWith: { prototype in
          system.pointee.insert(
            RelationalConstraint(prototype: prototype, at: ConstraintLocator(newRef)))
        })

      assert(!newRef.type.hasTypeParams)
      return newRef
    }

    assert(matches.count == 1)
    return TypeDeclRefExpr(decl: matches.types[0], range: ref.range)
  }

}
