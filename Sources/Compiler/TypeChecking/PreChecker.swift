/// An AST visitor that resolves unqualified identifiers, realizes type signatures and desugars
/// some expressions, before constraint generation.
struct PreChecker: NodeWalker {

  typealias Result = Bool

  var parent: Node?

  /// The internal implementation of the expression visitor.
  private var impl: PreCheckerImpl

  /// A Boolean value that indicates whether the walker encountered errors.
  var hasErrors = false

  init(system: UnsafeMutablePointer<ConstraintSystem>, useSite: DeclSpace) {
    impl = PreCheckerImpl(system: system, useSite: useSite)
  }

  var innermostSpace: DeclSpace? {
    get { impl.useSite }
    set { impl.useSite = newValue }
  }

  mutating func willVisit(_ expr: Expr) -> (shouldWalk: Bool, nodeBefore: Expr) {
    switch expr {
    case let expr as TupleExpr:
      // Substitute `e` for `(e)`, effectively eliminating parenthesized expressions.
      if (expr.elems.count == 1) && (expr.elems[0].label == nil) {
        return (true, expr.elems[0].value)
      } else {
        return (true, expr)
      }

    case let expr as LambdaExpr:
      // Realize a type for the lambda using potential information from its signature.
      expr.type = expr.realize()

      // If the lambda's body is a single expression, prepare it for the constraint generator.
      // Otherwise, it will be type checked after we've inferred the type of the signature.
      return (expr.decl.singleExprBody != nil, expr)

    case let expr as MatchExpr:
      // Match expressions require special handling to deal with the bindings declared as patterns.
      let newExpr = impl.visit(expr)
      hasErrors = hasErrors || newExpr.type[.hasErrors]
      return (false, newExpr)

    case is ErrorExpr:
      return (false, expr)

    default:
      return (true, expr)
    }
  }

  mutating func didVisit(_ expr: Expr) -> (shouldContinue: Bool, nodeAfter: Expr) {
    let newExpr = expr.accept(&impl)
    hasErrors = newExpr.type[.hasErrors] || hasErrors
    return (true, newExpr)
  }

}

/// The internal pre-checker visitor.
fileprivate struct PreCheckerImpl: ExprVisitor {

  typealias ExprResult = Expr

  /// A pointer to the system in which new constraints are inserted.
  let system: UnsafeMutablePointer<ConstraintSystem>

  /// The declaration space in which the visited expression resides.
  var useSite: DeclSpace?

  func visit(_ node: BoolLiteralExpr) -> Expr {
    return node
  }

  func visit(_ node: IntLiteralExpr) -> Expr {
    return node
  }

  func visit(_ node: FloatLiteralExpr) -> Expr {
    return node
  }

  func visit(_ node: StringLiteralExpr) -> Expr {
    return node
  }

  func visit(_ node: AssignExpr) -> Expr {
    return node
  }

  func visit(_ node: BaseCastExpr) -> Expr {
    // Contextualize the type signature on the RHS.
    guard let rhs = TypeChecker.contextualize(
      sign: node.sign, from: useSite!, system: &system.pointee)
    else {
      return ErrorExpr(type: node.type.context.errorType, range: node.range)
    }

    // Cast expressions are always assumed to have the type denoted by the signature.
    node.type = rhs
    return node
  }

  func visit(_ node: StaticCastExpr) -> Expr {
    return visit(node as BaseCastExpr)
  }

  func visit(_ node: RuntimeCastExpr) -> Expr {
    return visit(node as BaseCastExpr)
  }

  func visit(_ node: PointerCastExpr) -> Expr {
    return visit(node as BaseCastExpr)
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

  /// Resolves an `UnresolvedDeclRefExpr`.
  ///
  /// The name is resolved with an unqualified lookup from the declaration space in which the
  /// reference resides. Implicit member references are rewritten with an explicit `self`.
  func visit(_ node: UnresolvedDeclRefExpr) -> Expr {
    let context = node.type.context

    var forwardDecl: TypeOrValueDecl?
    var matches: LookupResult

    // If we're in a brace statement, look for local bindings first.
    if useSite is BraceStmt {
      matches = useSite!.lookup(qualified: node.ident.base)

      if let decl = matches.values.first(where: { !$0.isOverloadable }) {
        // We found a local, non-overloadable value declaration. That should be either a parameter
        // or a local variable. We're done if it syntactically before the identifier. Otherwise, we
        // must check in outer spaces for a better candidate.
        let declRange = (decl as? VarDecl)?.patternBindingDecl?.range ?? decl.range
        if let declLoc = declRange?.upperBound,
           let nodeLoc = node.range?.lowerBound,
           nodeLoc >= declLoc
        {
          return bind(ref: node, to: LookupResult(types: [], values: [decl]))
        } else {
          matches.values.removeAll()
          matches.types.removeAll()
          forwardDecl = decl
        }
      } else if !matches.types.isEmpty {
        // We found one local type declaration. We're done.
        matches.values.removeAll()
        return bind(ref: node, to: matches)
      }

      let outer = useSite!.parentDeclSpace!.lookup(
        unqualified: node.ident.base, in: node.type.context)
      matches.append(contentsOf: outer)
    } else {
      matches = useSite!.lookup(unqualified: node.ident.base, in: node.type.context)
    }

    guard !matches.isEmpty else {
      if forwardDecl != nil {
        DiagDispatcher.instance.report(
          .useOfLocalBindingBeforeDeclaration(symbol: node.ident.base, range: node.range))
      } else {
        DiagDispatcher.instance.report(
          .cannotFind(symbol: node.ident.base, range: node.range))
      }
      return ErrorExpr(type: context.errorType, range: node.range)
    }

    return bind(ref: node, to: matches)
  }

  /// Resolves an `UnresolvedQualDeclRefExpr`.
  ///
  /// The name is resolved with a qualified lookup from the base of the reference.
  func visit(_ node: UnresolvedQualDeclRefExpr) -> Expr {
    let context = node.type.context

    let baseType = node.namespace.realize(unqualifiedFrom: useSite!)
    guard !baseType.isError else {
      // The diagnostic is emitted by the failed attempt to realize the base.
      return ErrorExpr(type: context.errorType, range: node.range)
    }

    // Handle `T::self`.
    if node.ident.base == "self" {
      return KindRefExpr(type: baseType.kind, range: node.range)
    }

    // Handle built-ins.
    if baseType === context.builtin.instanceType {
      guard let decl = context.getBuiltinDecl(for: node.ident.base) else {
        DiagDispatcher.instance.report(
          .cannotFind(builtin: node.ident.base, range: node.identRange))
        return ErrorExpr(type: context.errorType, range: node.range)
      }

      // There's no need to contextualize the type, built-ins are never generic.
      return DeclRefExpr(decl: decl, type: decl.realize(), range: node.range)
    }

    // Run a qualified lookup if the namespace resolved to a nominal type.
    let matches = baseType.lookup(member: node.ident.base)
    guard !matches.isEmpty else {
      DiagDispatcher.instance.report(
        .cannotFind(member: String(describing: node.ident), in: baseType, range: node.identRange))
      return ErrorExpr(type: context.errorType, range: node.identRange)
    }

    // Note that this will throw the type signature away.
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

  func visit(_ node: KindRefExpr) -> Expr {
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

  /// Resolves a `SpecializedDeclRefExpr`.
  func visit(_ node: SpecializedDeclRefExpr) -> ExprResult {
    fatalError("not implemented")
  }

  func visit(_ node: LambdaExpr) -> Expr {
    return node
  }

  func visit(_ node: AsyncExpr) -> Expr {
    if let sign = node.body.retSign {
      let retType = sign.realize(unqualifiedFrom: useSite!)
      node.body.type = node.type.context.funType(params: [], retType: retType)
      node.body.state = .realized
    }

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
    let success = TypeChecker.check(expr: &node.subject, useSite: useSite!)

    // Bail out if the subjet doesn't have a valid type.
    let context = node.type.context
    guard success else {
      return ErrorExpr(type: context.errorType, range: node.range)
    }

    var driver = PreChecker(system: system, useSite: useSite!)
    driver.parent = node

    for `case` in node.cases {
      // Type check each case pattern.
      var cs = ConstraintSystem()
      let result = TypeChecker.check(
        pattern: `case`.pattern,
        fixedType: node.subject.type,
        useSite: useSite!,
        system: &cs)
      guard result.errors.isEmpty else {
        return ErrorExpr(type: context.errorType, range: node.range)
      }

      // If the match is a sub-expression, make sure that its cases are single expressions.
      guard !node.isSubexpr || (`case`.body.stmts.count == 1) && (`case`.body.stmts[0] is Expr)
      else {
        DiagDispatcher.instance.report(.multipleStatementInMatchExpression(range: `case`.range))
        return ErrorExpr(type: context.errorType, range: node.range)
      }
      driver.walk(stmt: `case`.body)
    }

    // If the match is a not a sub-expression, just type it as `Unit`.
    if !node.isSubexpr {
      node.type = context.unitType
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
      DiagDispatcher.instance.report(
        .cannotFind(member: "new", in: call.fun.type, range: call.fun.range))
      return newCall(newFunExpr: ErrorExpr(type: context.errorType, range: call.fun.range))
    }

    // Substitute the callee for an overload set.
    let newFunExpr = bind(ref: call.fun, to: matches)
    assert(!(newFunExpr is TypeDeclRefExpr))

    // Desugar the constructor call.
    return newCall(newFunExpr: newFunExpr)
  }

  /// Binds a declaration reference to a lookup result, rewriting the expression accordingly.
  func bind(ref: Expr, to matches: LookupResult) -> Expr {
    // Favor values over types.
    if matches.values.count > 1 {
      assert(!ref.type[.hasVariables])
      let unresolved = ref.type.context.unresolvedType
      return OverloadedDeclRefExpr(
        subExpr: ref, declSet: matches.values, type: unresolved, range: ref.range)
    }

    if let decl = matches.values.first {
      // Rewrite the given expression as an explicit member reference.
      let newRef: Expr
      if let expr = ref as? MemberExpr {
        // Preserve the base expr.
        newRef = MemberDeclRefExpr(base: expr.base, decl: decl, type: ref.type, range: expr.range)
      } else if decl.isMember {
        // Desugar an implicit reference to `self`.
        let matches = useSite!.lookup(unqualified: "self", in: decl.type.context)
        let selfDecl = matches.values[0]
        let selfExpr = DeclRefExpr(decl: selfDecl, type: selfDecl.type, range: ref.range)
        selfExpr.type = TypeChecker.contextualize(
          decl: selfDecl,
          from: useSite!,
          processingContraintsWith: {
            system.pointee.insert(prototype: $0, at: ConstraintLocator(selfExpr))
          })

        newRef = MemberDeclRefExpr(base: selfExpr, decl: decl, type: ref.type, range: ref.range)
      } else {
        newRef = DeclRefExpr(decl: decl, type: ref.type, range: ref.range)
      }

      // Contextualize the declaration's type in case it is generic.
      newRef.type = TypeChecker.contextualize(
        decl: decl,
        from: useSite!,
        processingContraintsWith: {
          system.pointee.insert(prototype: $0, at: ConstraintLocator(newRef))
        })

      // Erase parameter passing policies from the type of parameter declaration references.
      if let paramType = newRef.type as? FunParamType {
        assert(decl is FunParamDecl)
        newRef.type = paramType.rawType
      }

      assert(!newRef.type[.hasTypeParams])
      return newRef
    }

    return TypeDeclRefExpr(decl: matches.types[0], range: ref.range)
  }

}
