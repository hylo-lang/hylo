import AST

/// Val's type checker.
///
/// The type checker serves two purposes: first, it verifies that program sources satisfy Val's
/// (flow-insensitive) static type system; second, it resolves type and variable identifiers to
/// their declaration.
///
/// Conceptually, type checking is a composition of five phases:
/// - **Extension binding**
///   Binds extensions to the declaration they extend.
/// - **Conformance enumeration**
///   Gather the explicit, inherited and synthesized conformances for all nominal types.
/// - **Generic environment realization**
///   Realizes generic type signatures, establishing equivalence classes and conformance relations
///   for each generic type parameter.
/// - **Name resolution**
///   Resolves type and variable identifiers to their declaration.
/// - **Semantic type checking**
///   Checks that a particular declaration satisfies Val's type system.
///
/// These phases cannot be performed sequentially, as some operations may require results from
/// "later" phases. Thus, the process is carried out lazily, so that not all of the AST need to be
/// brought up to a particular phase at the same time.
///
/// Type checking is "declaration-driven". We start from a declaration (e.g., a module) and visit
/// all nested declaration recursively. Dependencies are not eagerly type checked. Instead, we move
/// them at the minimal "phase" that satisfies the requirements of the whathever we are processing.
///
/// The "phase" at which a particular node sits is encoded either explicitly as node properties, or
/// by the class of the node itself. For instance, an `UnresolvedDeclRefExpr` will be substituted
/// for a `DeclRefExpr` after name resolution.
public enum TypeChecker {

  /// Initializes the type checker.
  public static func initialize(in context: AST.Context) {
    context.prepareGenericEnv = prepareGenericEnv(env:)
  }

  /// Type checks the given declaration. This is the main entry point into the type checker.
  ///
  /// - Parameter decl: The declaration to type check.
  public static func check(decl: Decl) -> Bool {
    decl.accept(DeclChecker())
  }

  /// Type checks the given statement.
  ///
  /// - Parameters:
  ///   - stmt: The statement to type check.
  ///   - useSite: The declaration space in which the statement is type checked.
  public static func check(stmt: Stmt, useSite: DeclSpace) {
    stmt.accept(StmtChecker(useSite: useSite))
  }

  /// Type checks the given expression.
  ///
  /// - Parameters:
  ///   - expr: The expression to type check.
  ///   - expectedType: The expected type of the expression, based on the context in which it
  ///     appears. For instance, the contextual type of `9` in `val x: UInt = 9` is `UInt`. No
  ///     assumption is made if it assigned to `nil`.
  ///   - useSite: The declaration space in which the expression is type checked.
  public static func check(
    expr: inout Expr,
    expectedType: ValType? = nil,
    useSite: DeclSpace
  ) {
    var system = ConstraintSystem()
    check(expr: &expr, expectedType: expectedType, useSite: useSite, system: &system)
  }

  public static func prepareGenericEnv(env: GenericEnv) -> Bool {
    // Generate equivalence classes.
    var solver = TRSolver()
    guard solver.solve(typeReqs: env.typeReqs, from: env.space) else { return false }
    env.equivalences = solver.computeEquivalenceClasses(env: env)

    // Process type requirements.
    for req in env.typeReqs {
      // Realize each operand's signature.
      let lhs = req.lhs.realize(unqualifiedFrom: env.space)
      let rhs = req.rhs.realize(unqualifiedFrom: env.space)

      // Skip the requirement if either of the types has an error.
      guard !lhs.hasErrors && !rhs.hasErrors else { continue }

      switch req.kind {
      case .equality:
        env.constraintPrototypes.append(.equality(lhs: lhs, rhs: rhs))

      case .conformance:
        if registerConformance(lhs, rhs, env, req) {
          env.constraintPrototypes.append(.conformance(lhs: lhs, rhs: rhs as! ViewType))
        }
      }
    }

    return true
  }

  /// Contextualizes the type of the given declaration from the specified use site.
  ///
  /// - Parameters:
  ///   - decl: The declaration to contextualize.
  ///   - useSite: The declaration space from which the `decl` is being referred.
  ///   - args: A substitution table mapping generic type parameters to specialization arguments.
  ///   - handleConstraint: A closure that accepts contextualized contraint prototypes generated
  ///     for each opened generic associated with type requirements.
  /// - Returns: The contextualized type of the declaration.
  public static func contextualize(
    decl: ValueDecl,
    from useSite: DeclSpace,
    args: [GenericParamType: ValType] = [:],
    processingContraintsWith handleConstraint: (GenericEnv.ConstraintPrototype) -> Void = { _ in }
  ) -> ValType {
    // Realize the declaration's type.
    var genericType: ValType
    switch decl {
    case _ where decl.state >= .realized:
      genericType = decl.type

    case let varDecl as VarDecl:
      // Variable declarations must be type checked, as in their type might be inferred.
      _ = check(decl: varDecl)
      genericType = varDecl.type

      // FIXME: We could slightly more clever and avoid fully type checking annotated declarations
      // by simply realizing their signature.

    case let ctorDecl as CtorDecl where ctorDecl.isSynthesized:
      // Synthesized constructors can't be realized until all stored properties of the constructed
      // type have been type checked.
      let typeDecl = ctorDecl.parentDeclSpace as! NominalTypeDecl
      assert(typeDecl.storedVars.count == ctorDecl.params.count)
      for (varDecl, paramDecl) in zip(typeDecl.storedVars, ctorDecl.params) {
        // Type check the variable declaration.
        guard check(decl: varDecl) else {
          ctorDecl.setState(.invalid)
          return decl.type.context.errorType
        }

        // Assign the type of the declaration to its corresponding parameter.
        paramDecl.type = varDecl.type
        paramDecl.setState(.typeChecked)
      }

      // Once all parameters have been attributed a proper type, the constructor can be realized.
      genericType = ctorDecl.realize()

    default:
      genericType = decl.realize()
    }

    // Specialize the generic parameters for which arguments have been provided.
    if !args.isEmpty {
      genericType = genericType.specialized(with: args)
    }

    // If the declaration's type doesn't contain free parameters, we're done.
    guard genericType.hasTypeParams else { return genericType }

    // If the declaration is a generic environment, we have to contextualize its own parameters
    // externally, regardless of the use-site. That situation denotes a "fresh" reference to a
    // generic declaration within its own space (e.g., a recursive call to a generic function).
    if let space = decl as? GenericDeclSpace {
      guard let env = space.prepareGenericEnv() else {
        return decl.type.context.errorType
      }

      // Constructors are contextualized from outside of their type declaration.
      var adjustedSite: DeclSpace = space is CtorDecl
        ? space.spacesUpToRoot.first(where: { $0 is TypeDecl })!.parentDeclSpace!
        : space
      if adjustedSite.isDescendant(of: useSite) {
        adjustedSite = useSite
      }

      let (contextualType, _) = env.contextualize(
        genericType,
        from: adjustedSite,
        processingContraintsWith: handleConstraint)
      return contextualType
    }

    // Find the innermost generic space, relative to this declaration. We can assume there's one,
    // otherwise `realize()` would have failed to resolve the decl.
    guard let env = decl.parentDeclSpace!.innermostGenericSpace!.prepareGenericEnv() else {
      return decl.type.context.errorType
    }

    let (contextualType, _) = env.contextualize(
      genericType, from: useSite, processingContraintsWith: handleConstraint)
    return contextualType
  }

  // MARK: Internal API

  /// Type checks the given expression.
  ///
  /// - Parameters:
  ///   - expr: The expression to type check.
  ///   - expectedType: The expected type of the expression, based on the context in which it
  ///     appears. For instance, the contextual type of `9` in `val x: UInt = 9` is `UInt`. No
  ///     assumption is made if it assigned to `nil`.
  ///   - useSite: The declaration space in which the expression is type checked.
  ///   - system: A system with potential pre-existing constraints that should be solved together
  ///     with those related to the expression.
  /// - Returns: The best solution found by the type solver
  @discardableResult
  static func check(
    expr        : inout Expr,
    expectedType: ValType? = nil,
    useSite     : DeclSpace,
    system      : inout ConstraintSystem
  ) -> Solution {
    // Pre-check the expression to resolve unqualified identifiers, realize type signatures and
    // desugar constructor calls.
    withUnsafeMutablePointer(to: &system, { ptr in
      let driver = PreCheckDriver(system: ptr, useSite: useSite)
      (_, expr) = driver.walk(expr)
    })

    // Generate constraints from the expression.
    withUnsafeMutablePointer(to: &system, { ptr in
      let driver = CSGenDriver(system: ptr, useSite: useSite)
      _ = driver.walk(expr)
    })

    if let type = expectedType {
      // Since the type of a match expression is inferred as a supertype of all case statements, we
      // must create an equality constraint rather than a subtyping one, so that the solver does
      // not need to solve `A <: $T && $T <: B`.
      let kind: RelationalConstraint.Kind = (expr is MatchExpr) && (expr.type is TypeVar)
        ? .equality
        : .subtyping

      // Insert the expected type into the constraint system.
      system.insert(
        RelationalConstraint(kind: kind, lhs: expr.type, rhs: type, at: ConstraintLocator(expr)))
    }

    // Solve the constraint system.
    var solver = CSSolver(system: system, context: expr.type.context)
    let solution = solver.solve()

    // Report type errors.
    solution.reportAllErrors(in: expr.type.context)

    // Apply the solution.
    let dispatcher = TypeDispatcher(solution: solution)
    (_, expr) = dispatcher.walk(expr)

    return solution
  }

  /// Type checks the given pattern.
  @discardableResult
  static func check(
    pattern: Pattern,
    expectedType: ValType? = nil,
    useSite: DeclSpace,
    system: inout ConstraintSystem
  ) -> Solution {
    // Generate constraints from the pattern.
    withUnsafeMutablePointer(to: &system, { ptr in
      let driver = CSGenDriver(system: ptr, useSite: useSite)
      _ = driver.walk(pattern)
    })

    if let type = expectedType {
      system.insert(
        RelationalConstraint(
          kind: .subtyping, lhs: pattern.type, rhs: type, at: ConstraintLocator(pattern)))
    }

    // Solve the constraint system.
    var solver = CSSolver(system: system, context: pattern.type.context)
    let solution = solver.solve()

    // Report type errors.
    solution.reportAllErrors(in: pattern.type.context)

    // Apply the solution.
    let dispatcher = TypeDispatcher(solution: solution)
    _ = dispatcher.walk(pattern)

    return solution
  }

  /// Contextualizes the type of a the given type signature.
  ///
  /// - Parameters:
  ///   - sign: The type signature to contextualize.
  ///   - useSite: The declaration space in which the signature is type checked.
  ///   - system: A system with potential pre-existing constraints that should be solved together
  ///     with those related to the signature.
  /// - Returns: The contextualized type of the declaration if it is valid; otherwise, `nil`.
  static func contextualize(
    sign: Sign,
    from useSite: DeclSpace,
    system: inout ConstraintSystem
  ) -> ValType? {
    // Realize the signature, generating diagnostics as necessary.
    var signType = sign.realize(unqualifiedFrom: useSite)
    assert(!signType.hasUnresolved)

    // Bail out if the signature is invalid.
    guard !signType.hasErrors else { return nil }

    if signType.hasTypeParams {
      // The signature is generic; we have to contextualize its parameters. We can assume that
      // there's a declaration space from the use-site, otherwise `realize()` would have failed to
      // resolve the signature.
      guard let env = useSite.innermostGenericSpace!.prepareGenericEnv() else { return nil }

      // Contextualize the signature.
      (signType, _) = env.contextualize(
        signType,
        from: useSite,
        processingContraintsWith: { system.insert(prototype: $0, at: ConstraintLocator(sign)) })
    }

    // Check if we have to synthetize additional generic arguments, in case the signature refers
    // to an "underspecialized" generic nominal type.
    return completeGenericArgs(type: signType, system: &system, locator: ConstraintLocator(sign))
  }

  /// Completes the argument list of an "underspecified" generic nominal type.
  ///
  /// If `type` is a generic nominal type with too few generic arguments, this method produces a
  /// bound generic type in which all missing arguments are replaced by opened parameters. This
  /// requires that `type` be a bare nominal type, or a bound generic type with fewer arguments
  /// than the number of parameters of its declaration.
  ///
  /// - Parameters:
  ///   - type: Either a bare nominal type or a bound generic type.
  ///   - system: The system in which constraints on opened type parameters are inserted.
  ///   - locator: A locator for all generated constraints.
  /// - Returns: A bound generic type filling out missing generic arguments with fresh type
  ///   variables, if `type` is an underspecified generic nominal type; otherwise, `nil`.
  static func completeGenericArgs(
    type: ValType, system: inout ConstraintSystem, locator: ConstraintLocator
  ) -> ValType? {
    guard let nominalType = type as? NominalType,
          let clause = nominalType.decl.genericClause
    else { return type }

    // Complete the argument list if necessary.
    var args = (nominalType as? BoundGenericType)?.args ?? []
    guard args.count < clause.params.count else { return type }

    args.append(contentsOf: clause.params.dropFirst(args.count).map({ $0.instanceType }))
    guard let env = nominalType.decl.prepareGenericEnv() else {
      return nil
    }

    let boundType = nominalType.context.boundGenericType(decl: nominalType.decl, args: args)
    let (contextualType, _) = env.contextualize(
      boundType,
      from: nominalType.decl.rootDeclSpace,
      processingContraintsWith: { system.insert(prototype: $0, at: locator) })
    return contextualType
  }

  /// Recursively assigns the given type to a pattern and its sub-patterns, generating diagnostics
  /// upon failure.
  ///
  /// - Parameters:
  ///   - type: A type.
  ///   - pattern: A pattern.
  /// - Returns: `true` if the type was successfully applied, or `false` if its layout does not
  ///   match that of the pattern.
  static func assign(type: ValType, to pattern: Pattern) -> Bool {
    switch pattern {
    case let namedPattern as NamedPattern:
      namedPattern.type = type
      namedPattern.decl.type = type.uncontextualized
      return true

    case let tuplePattern as TuplePattern:
      if tuplePattern.elems.count > 1 {
        guard let tupleType = type as? TupleType,
              tupleType.elems.count == tuplePattern.elems.count
        else {
          type.context.report(.wrongTuplePatternLength(type: type, range: pattern.range))
          return false
        }

        tuplePattern.type = type
        return zip(tupleType.elems, tuplePattern.elems).allSatisfy({ (typeElem, patterElem) in
          assign(type: typeElem.type, to: patterElem.pattern)
        })
      }

      assert(tuplePattern.elems.count == 1)
      tuplePattern.type = type
      tuplePattern.elems[0].pattern.type = type
      return true

    case is WildcardPattern:
      pattern.type = type
      return true

    default:
      fatalError("unreachable")
    }
  }

  private static func registerConformance(
    _ lhs: ValType, _ rhs: ValType, _ env: GenericEnv, _ req: TypeReq
  ) -> Bool {
    let context = lhs.context

    guard let view = rhs as? ViewType else {
      context.report(.nonViewTypeConformanceRequirement(type: rhs, range: req.rhs.range))
      return false
    }
    let viewDecl = view.decl as! ViewTypeDecl

    // Be sure not to register the same conformance twice.
    guard env.conformance(of: lhs, to: view) == nil else {
      assert(env.equivalences
              .equivalenceClass(containing: lhs)?
              .allSatisfy({ env.conformance(of: $0, to: view) != nil })
              ?? true)
      return true
    }

    // Register the conformance for all members of `lhs`'s equivalence class, provided they all are
    // either generic parameters or associated types defined in the same environment.
    let conformance = ViewConformance(viewDecl: viewDecl, range: req.rhs.range)
    if let equivalenceClass = env.equivalences.equivalenceClass(containing: lhs) {
      for type in equivalenceClass {
        guard env.defines(type: type) else {
          context.report(.illegalConformanceRequirement(type: lhs, range: req.lhs.range))
          return false
        }
        env.insert(conformance: conformance, for: type)
      }
    } else {
      guard env.defines(type: lhs) else {
        context.report(.illegalConformanceRequirement(type: lhs, range: req.lhs.range))
        return false
      }
      env.insert(conformance: conformance, for: lhs)
    }

    // View conformance successfully registered.
    return true
  }

}
