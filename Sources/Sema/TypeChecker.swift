import AST

/// Val's type checker.
///
/// This class is the entry point to Val's type checker. Its purpose is twofold: determine whether
/// program sources satisfy Val's (flow-insensitive) static type system; resolve type and variable
/// identifiers to their declaration.
///
/// Conceptually, type checking is a composition of multiple phases:
/// - **Extension binding**
///   Binds extensions to the declaration they extend.
/// - **Conformance enumeration**
///   Initializes the view conformance set of all nominal types. This includes listing inherited
///   and synthetized conformances.
/// - **Generic environment realization**
///   Realizes generic type signatures, establishing equivalence classes and conformance relations
///   for each generic type parameter.
/// - **Name resolution**
///   Resolves type and variable identifiers to their declaration.
/// - **Semantic type checking**
///   Checks that a particular declaration satisfies Val's type system.
///
/// These phases cannot be performed completely sequentially, because some operations may require
/// results from "later" phases. For instance, binding the extension of a member type requires to
/// run name resolution over a qualified type reference (e.g., `Foo::Bar`). Thus, the process has
/// to be carried out lazily, so that not all of the AST need to be brought up to a particular
/// phase at the same time.
///
/// The process is "declaration-driven"; it starts at a declaration node (e.g., a `ModuleDecl`) and
/// visits all nested declarations recursively. Dependencies are not fully type checked. Instead,
/// the type checker aims to move them at the minimal "phase" that satisfies the requirements of
/// the construction it is checking. For instance, referring to a method in another type does not
/// triggers said type to be fully type checked; it is sufficient to build its member lookup table
/// and realize the signature of the method.
///
/// The "phase" at which a particular node sits is encoded by the node itself, either explicitly
/// within its properties or by its very type (e.g., name resolution substitutes `DeclRefExpr`s for
/// `UnresolvedDeclRefExpr`s).
public final class TypeChecker {

  public init(context: AST.Context) {
    self.context = context

    // Configure the context.
    context.prepareGenericEnv = prepareGenericEnv(env:)
  }

  /// The context in which the pass runs.
  public unowned let context: AST.Context

  /// Type checks the given declaration. This is the main entry point into the type checker.
  ///
  /// - Parameter decl: The declaration to type check.
  public func check(decl: Decl) -> Bool {
    decl.accept(DeclChecker(checker: self))
  }

  /// Type checks the given statement.
  ///
  /// - Parameters:
  ///   - stmt: The statement to type check.
  ///   - useSite: The declaration space in which the statement is type checked.
  public func check(stmt: Stmt, useSite: DeclSpace) {
    stmt.accept(StmtChecker(checker: self, useSite: useSite))
  }

  /// Type checks the given expression.
  ///
  /// - Parameters:
  ///   - expr: The expression to type check.
  ///   - expectedType: The expected type of the expression, based on the context in which it
  ///     appears. For instance, the contextual type of `9` in `val x: UInt = 9` is `UInt`. No
  ///     assumption is made if it assigned to `nil`.
  ///   - useSite: The declaration space in which the expression is type checked.
  public func check(
    expr: inout Expr,
    expectedType: ValType? = nil,
    useSite: DeclSpace
  ) {
    var system = ConstraintSystem()
    check(expr: &expr, expectedType: expectedType, useSite: useSite, system: &system)
  }

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
  ///
  /// - Returns: The best solution found by the type solver
  @discardableResult
  func check(
    expr: inout Expr,
    expectedType: ValType? = nil,
    useSite: DeclSpace,
    system: inout ConstraintSystem
  ) -> Solution {
    // Pre-check the expression.
    // This resolves primary names, realizes type rerps and desugars constructor calls.
    withUnsafeMutablePointer(to: &system, { ptr in
      let driver = PreCheckDriver(system: ptr, checker: self, useSite: useSite)
      (_, expr) = driver.walk(expr)
    })

    // Generate constraints from the expression.
    withUnsafeMutablePointer(to: &system, { ptr in
      let driver = CSGenDriver(system: ptr, useSite: useSite)
      _ = driver.walk(expr)
    })

    if let type = expectedType {
      system.insert(
        RelationalConstraint(
          kind: .subtyping, lhs: expr.type, rhs: type, at: ConstraintLocator(expr)))
    }

    // Solve the constraint system.
    var solver = CSSolver(system: system, checker: self)
    let solution = solver.solve()

    // Report type errors.
    TypeErrorReporter(context: context, solution: solution).report(solution.errors)

    // Apply the solution.
    let dispatcher = TypeDispatcher(solution: solution)
    (_, expr) = dispatcher.walk(expr)

    return solution
  }

  public func prepareGenericEnv(env: GenericEnv) -> Bool {
    // Generate equivalence classes.
    var solver = TRSolver()
    guard solver.solve(typeReqs: env.typeReqs, from: env.space) else { return false }
    env.equivalences = solver.computeEquivalenceClasses(env: env)

    // Process type requirements.
    for req in env.typeReqs {
      // Realize each operand's type representation.
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

  /// Contextualizes the type the given declaration from the specified use site.
  ///
  /// - Parameters:
  ///   - decl: The declaration to contextualize.
  ///   - useSite: The declaration space from which the `decl` is being referred.
  ///   - args: A dictionary containing specialization arguments for generic type parameters.
  ///   - handleConstraint: A closure that accepts contextualized contraint prototypes. It is
  ///     called if the contextualized type contains opened generic parameters for which there
  ///     exist type requirements.
  ///
  /// - Returns: The contextualized type of the declaration.
  public func contextualize(
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

    // If the declaration's type does not contain any free parameter, our work is done.
    guard genericType.hasTypeParams else { return genericType }

    // If the declaration is its own generic environment, then we must contextualize it externally,
    // regardless of the use-site. This situation corresponds to a "fresh" use of a generic
    // declaration within its own space (e.g., a recursive call to a generic function).
    if let gds = decl as? GenericDeclSpace {
      guard let env = gds.prepareGenericEnv() else {
        return decl.type.context.errorType
      }

      // Adjust the use site depending on the type of declaration.
      var adjustedSite: DeclSpace
      if gds is CtorDecl {
        // Constructors are contextualized from outside of their type declaration.
        adjustedSite = gds.spacesUpToRoot.first(where: { $0 is TypeDecl })!.parentDeclSpace!
      } else {
        adjustedSite = gds
      }
      if adjustedSite.isDescendant(of: useSite) {
        adjustedSite = useSite
      }

      return env.contextualize(
        genericType, from: adjustedSite, processingContraintsWith: handleConstraint)
    }

    // Find the innermost generic space, relative to this declaration. We can assume there's one,
    // otherwise `realize()` would have failed to resolve the decl.
    guard let env = decl.parentDeclSpace!.innermostGenericSpace!.prepareGenericEnv() else {
      return decl.type.context.errorType
    }
    return env.contextualize(
      genericType, from: useSite, processingContraintsWith: handleConstraint)
  }

  private func registerConformance(
    _ lhs: ValType, _ rhs: ValType, _ env: GenericEnv, _ req: TypeReq
  ) -> Bool {
    // Complain if the left operand is not a generic parameter.
    // FIXME: Handle dependent types.
    guard let param = lhs as? GenericParamType, env.params.contains(param) else {
      context.report(.illegalConformanceRequirement(type: lhs, range: req.lhs.range))
      return false
    }

    // Complain if the right operand is not a view.
    guard let view = rhs as? ViewType else {
      context.report(.nonViewTypeConformanceRequirement(type: rhs, range: req.rhs.range))
      return false
    }

    let viewDecl = view.decl as! ViewTypeDecl
    let skolem = env.skolemize(param)
    if let types = env.equivalences.equivalenceClass(containing: skolem) {
      // The skolem belongs to an equivalence class; register the new conformance for every member
      // of the skolem's equivalence class.
      for type in types {
        guard let skolem = type as? SkolemType,
              skolem.genericEnv === env
        else {
          // Complain that the equivalence class contains non-generic members.
          context.report(.illegalConformanceRequirement(type: lhs, range: req.lhs.range))
          return false
        }

        guard env.conformance(of: skolem, to: view) == nil else { return false }
        env.insert(
          conformance: ViewConformance(viewDecl: viewDecl, range: req.rhs.range),
          for: skolem)
      }
    } else if env.conformance(of: skolem, to: view) == nil {
      // The skolem has no equivalence class; register the new conformance fot it directly.
      env.insert(
        conformance: ViewConformance(viewDecl: viewDecl, range: req.rhs.range),
        for: skolem)
    }

    return true
  }

  // MARK: Internal API

  /// Contextualizes the type of a the given type signature.
  ///
  /// - Parameters:
  ///   - repr: The type signature to contextualize.
  ///   - useSite: The declaration space in which the signature is type checked.
  ///   - system: A system with potential pre-existing constraints that should be solved together
  ///     with those related to the signature.
  ///
  /// - Returns: The contextualized type of the declaration if it is valid; otherwise, `nil`.
  static func contextualize(
    repr: TypeRepr,
    from useSite: DeclSpace,
    system: inout ConstraintSystem
  ) -> ValType? {
    // Realize the signature, generating diagnostics as necessary.
    var signType = repr.realize(unqualifiedFrom: useSite)
    assert(!signType.hasUnresolved)

    // Bail out if the signature is invalid.
    guard !signType.hasErrors else { return nil }

    if signType.hasTypeParams {
      // The signature is generic; we have to contextualize its parameters. We can assume that
      // there's a declaration space from the use-site, otherwise `realize()` would have failed to
      // resolve the type repr.
      guard let env = useSite.innermostGenericSpace!.prepareGenericEnv() else { return nil }

      // Contextualize the signature.
      signType = env.contextualize(
        signType, from: useSite,
        processingContraintsWith: { prototype in
          system.insert(RelationalConstraint(prototype: prototype, at: ConstraintLocator(repr)))
        })
    }

    // Check if we have to synthetize additional generic arguments, in case the signature refers
    // to an "underspecialized" generic nominal type.
    return completeGenericArgs(
      type: signType, system: &system, locator: ConstraintLocator(repr))
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
  ///
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

    let newType = nominalType.context.boundGenericType(decl: nominalType.decl, args: args)
    return env.contextualize(
      newType, from: nominalType.decl.rootDeclSpace,
      processingContraintsWith: { prototype in
        system.insert(RelationalConstraint(prototype: prototype, at: locator))
      })
  }

  /// Recursively assigns the given type to a pattern and its sub-patterns, generating diagnostics
  /// upon failure.
  ///
  /// - Parameters:
  ///   - type: A type.
  ///   - pattern: A pattern.
  ///
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

}
