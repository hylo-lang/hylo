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
/// - **Existential realization**
///   Realizes existential types from generic type signatures.
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
  public func check(decl: Decl) {
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
  func check(
    expr: inout Expr,
    expectedType: ValType? = nil,
    useSite: DeclSpace,
    system: inout ConstraintSystem
  ) {
    // Pre-check the expression.
    // This resolves primary names, realizes type rerps and desugars constructor calls.
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
      context.report(.invalidConformanceRequirement(type: rhs, range: req.rhs.range))
      return false
    }

    let viewDecl = view.decl as! ViewTypeDecl
    let existential = env.existential(of: param)
    if let types = env.equivalences.equivalenceClass(containing: existential) {
      // The existential belongs to an equivalence class; register the new conformance for every
      // member of the existential's equivalence class.
      for type in types {
        guard let existential = type as? ExistentialType,
              existential.genericEnv === env
        else {
          // Complain that the equivalence class contains non-generic members.
          context.report(.illegalConformanceRequirement(type: lhs, range: req.lhs.range))
          return false
        }

        guard env.conformance(of: existential, to: view) == nil else { return false }
        env.insert(
          conformance: ViewConformance(viewDecl: viewDecl, range: req.rhs.range),
          for: existential)
      }
    } else if env.conformance(of: existential, to: view) == nil {
      // The existential has no equivalence class; register the new conformance fot it directly.
      env.insert(
        conformance: ViewConformance(viewDecl: viewDecl, range: req.rhs.range),
        for: existential)
    }

    return true
  }

  // MARK: Internal API

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
    case is WildcardPattern:
      pattern.type = type
      return true

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

    default:
      fatalError("unreachable")
    }
  }

}
