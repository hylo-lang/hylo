import Core
import Utils

/// A constraint system solver.
struct ConstraintSolver {

  /// The solution of an exploration given a particular choice.
  private typealias Exploration<T> = (choice: T, solution: Solution)

  /// A closure diagnosing the failure of a constraint, using `m` to reify types and reading the
  /// outcome of constraint solving from `o`.
  private typealias DiagnoseFailure = (
    _ m: SubstitutionMap,
    _ o: [ConstraintOrigin: Outcome]
  ) -> Diagnostic

  /// The outcome of the solving of a type constraint.
  private enum Outcome {

    /// The constraint was solved.
    ///
    /// Information inferred from the constraint has been stored in the solver's state.
    case success

    /// The constraint was unsatisfiable.
    ///
    /// The constraint was in conflict with the information inferred by the solver. The payload
    /// is a closure that generates a diagnostic of the conflict.
    case failure(DiagnoseFailure)

    /// The constraint was broken into subordinate constraints.
    ///
    /// The payload is a non-empty array of subordinate constraints along with a closure that
    /// generates a diagnostic will be used to generate a diagnostic in case one of the
    /// subordinate constraints is unsatisfiable.
    case product([ConstraintOrigin], DiagnoseFailure)

    /// Returns the diagnosis constructor of `.failure` or `.product` payload.
    var dianoseFailure: DiagnoseFailure? {
      switch self {
      case .success:
        return nil
      case .failure(let f):
        return f
      case .product(_, let f):
        return f
      }
    }

  }

  /// The scope in which the constraints are solved.
  private let scope: AnyScopeID

  /// The fresh constraints to solve.
  private var fresh: [Constraint] = []

  /// The constraints that are currently stale.ß
  private var stale: [Constraint] = []

  private var results: [ConstraintOrigin: Outcome] = [:]

  /// The type assumptions of the solver.
  private var typeAssumptions = SubstitutionMap()

  /// The binding assumptions of the solver.
  private var bindingAssumptions: [NodeID<NameExpr>: DeclRef] = [:]

  /// The current penalties of the solver's solution.
  private var penalties: Int = 0

  /// The score of the best solution computed so far.
  private var bestScore = Solution.Score.worst

  /// Indicates whether this instance should log a trace.
  private let isLoggingEnabled: Bool

  /// The current indentation level for logging messages.
  private var indentation = 0

  /// Creates an instance that solves the constraints in `fresh` in `scope`.
  init<S: Sequence>(
    scope: AnyScopeID,
    fresh: S,
    loggingTrace isLoggingEnabled: Bool
  ) where S.Element == Constraint {
    self.scope = scope
    self.fresh = Array(fresh)
    self.isLoggingEnabled = isLoggingEnabled
  }

  /// Returns the best solution solving the constraints in `self` using `checker` to query type
  /// relations and resolve names.
  mutating func solution(_ checker: inout TypeChecker) -> Solution {
    solveConstraints(&checker)!
  }

  /// Returns the best solution solving the constraints in `self` using `checker` to query type
  /// relations and resolve names or `nil` if no solution with a score better than `self.bestScore`
  /// can be found.
  private mutating func solveConstraints(_ checker: inout TypeChecker) -> Solution? {
    logState()
    log("steps:")

    while let constraint = fresh.popLast() {
      // Make sure the current solution is still worth exploring.
      if score() > bestScore {
        log("- abort")
        return nil
      }

      log("- solve: \"\(constraint)\"")
      indentation += 1; defer { indentation -= 1 }
      log("actions:")

      switch constraint {
      case let c as ConformanceConstraint:
        setOutcome(solve(conformance: c, using: &checker), for: c.cause)
      case let c as LiteralConstraint:
        setOutcome(solve(literal: c, using: &checker), for: c.cause)
      case let c as EqualityConstraint:
        setOutcome(solve(equality: c, using: &checker), for: c.cause)
      case let c as SubtypingConstraint:
        setOutcome(solve(subtyping: c, using: &checker), for: c.cause)
      case let c as ParameterConstraint:
        setOutcome(solve(parameter: c, using: &checker), for: c.cause)
      case let c as MemberConstraint:
        setOutcome(solve(member: c, using: &checker), for: c.cause)
      case let c as FunctionCallConstraint:
        setOutcome(solve(functionCall: c, using: &checker), for: c.cause)
      case let c as DisjunctionConstraint:
        return solve(disjunction: c, using: &checker)
      case let c as OverloadConstraint:
        return solve(overload: c, using: &checker)
      default:
        unreachable()
      }

      if fresh.isEmpty { refreshLiteralConstraints() }
    }

    return formSolution()
  }

  /// Returns the currently known cost of the solution being computed.
  ///
  /// The cost of a solution increases monotonically when a constraint is eliminated.
  private func score() -> Solution.Score {
    .init(errorCount: results.keys.elementCount(where: iFailureRoot), penalties: penalties)
  }

  /// Returns `true` iff the constraint originating at `o` failed and isn't subordinate.
  private func iFailureRoot(_ o: ConstraintOrigin) -> Bool {
    (o.parent == nil) && (succeeded(o) == false)
  }

  /// Returns whether the constraint originating at `o` succeeded; `.none` indicates that the
  /// outcome of the constraint hasn't been computed yet.
  private func succeeded(_ o: ConstraintOrigin) -> ThreeValuedBit {
    switch results[o] {
    case nil:
      return nil
    case .some(.success):
      return true
    case .some(.failure):
      return false
    case .some(.product(let s, _)):
      return s.reduce(nil, { (r, k) in r && succeeded(k) })
    }
  }

  /// Records the outcome `value` for the constraint originating at `key`.
  private mutating func setOutcome(_ value: Outcome?, for key: ConstraintOrigin) {
    switch value {
    case nil:
      log("- defer")
    case .some(.success):
      log("- success")
    case .some(.failure):
      log("- failure")
    case .some(.product):
      log("- break")
    }
    results[key] = value
  }

  /// Returns either `.success` if `c.subject` conforms to `c.traits`, `.failure` if it doesn't, or
  /// `nil` if neither of these outcomes can be be determined yet.
  private mutating func solve(
    conformance c: ConformanceConstraint,
    using checker: inout TypeChecker
  ) -> Outcome? {
    let goal = c.modifyingTypes({ typeAssumptions[$0] })

    var missingTraits: Set<TraitType>
    switch goal.subject.base {
    case is TypeVariable:
      postpone(goal)
      return nil

    case is BuiltinType:
      // Built-in types are `Sinkable`.
      missingTraits = c.traits.subtracting(
        [checker.ast.coreTrait(named: "Sinkable")!])

    default:
      missingTraits = goal.traits.subtracting(
        checker.conformedTraits(of: goal.subject, in: scope) ?? [])
    }

    if missingTraits.isEmpty {
      return .success
    } else {
      var subordinates: [ConstraintOrigin] = []
      for (i, t) in missingTraits.enumerated() {
        let s = c.cause.subordinate(i)
        let o = Outcome.failure { (m, _) in
          .error(m.reify(c.subject), doesNotConformTo: t, at: c.cause.site)
        }
        setOutcome(o, for: s)
        subordinates.append(s)
      }
      return .product(subordinates) { (_, _) in fatalError() }
    }
  }

  /// Returns either `.success` if `c.subject` conforms to `c.literalTrait`, `.failure` if it
  /// doesn't, or `nil` if neither of these outcomes can be determined yet.
  private mutating func solve(
    literal c: LiteralConstraint,
    using checker: inout TypeChecker
  ) -> Outcome? {
    let goal = c.modifyingTypes({ typeAssumptions[$0] })
    if checker.relations.areEquivalent(goal.subject, goal.defaultSubject) {
      return .success
    }

    if goal.subject.base is TypeVariable {
      postpone(goal)
      return nil
    }

    let t = checker.conformedTraits(of: goal.subject, in: scope) ?? []
    if t.contains(goal.literal) {
      // Add a penalty if `L` isn't `D`.
      penalties += 1
      return .success
    } else {
      return .failure { (m, _) in
        .error(m.reify(c.subject), doesNotConformTo: c.literal, at: c.cause.site)
      }
    }
  }

  /// Returns eiteher `.success` if `c.left` is unifiable with `c.right` or `.failure` otherwise.
  private mutating func solve(
    equality c: EqualityConstraint,
    using checker: inout TypeChecker
  ) -> Outcome {
    if unify(c.left, c.right, querying: checker.relations) {
      return .success
    } else {
      return .failure { (m, _) in
        let (l, r) = (m.reify(c.left), m.reify(c.right))
        return .error(type: l, incompatibleWith: r, at: c.cause.site)
      }
    }
  }

  /// Returns either `.success` if `c.left` is (strictly) subtype of `c.right`, `.failure` if it
  /// isn't, `.product` if `c` must be broken down to smaller constraints, or `nil` if that can't
  /// be determined yet.
  private mutating func solve(
    subtyping c: SubtypingConstraint,
    using checker: inout TypeChecker
  ) -> Outcome? {
    let goal = c.modifyingTypes({ typeAssumptions[$0] })
    if checker.relations.areEquivalent(goal.left, goal.right) {
      return goal.isStrict ? .failure(failureToSolve(c)) : .success
    }

    switch (goal.left.base, goal.right.base) {
    case (_, _ as TypeVariable):
      // The type variable is above a more concrete type. We should compute the "join" of all types
      // to which `L` is coercible and that are below `R`, but that set is unbounded. We have no
      // choice but to postpone the constraint.
      if goal.isStrict {
        postpone(goal)
      } else {
        schedule(inferenceConstraint(goal.left, isSubtypeOf: goal.right, because: goal.cause))
      }
      return nil

    case (_ as TypeVariable, _):
      // The type variable is below a more concrete type. We should compute the "meet" of all types
      // coercible to `R` and that are above `L`, but that set is unbounded unless `R` is a leaf.
      // If it isn't, we have no choice but to postpone the constraint.
      if goal.right.isLeaf {
        return unify(goal.left, goal.right, querying: checker.relations)
          ? .success
          : .failure(failureToSolve(c))
      } else if goal.isStrict {
        postpone(goal)
        return nil
      } else {
        schedule(inferenceConstraint(goal.left, isSubtypeOf: goal.right, because: goal.cause))
        return nil
      }

    case (_, _ as ExistentialType):
      // All types conform to any.
      if goal.right == .any { return .success }
      fatalError("not implemented")

    case (let l as LambdaType, let r as LambdaType):
      if !l.labels.elementsEqual(r.labels) {
        return .failure(failureToSolve(c))
      }
      if !unify(l.environment, r.environment, querying: checker.relations) {
        return .failure(failureToSolve(c))
      }

      // Parameters are contravariant; return types are covariant.
      var subordinates: [ConstraintOrigin] = []
      for i in 0 ..< l.inputs.count {
        let (a, b) = (r.inputs[i].type, l.inputs[i].type)
        subordinates.append(
          schedule(SubtypingConstraint(a, b, because: c.cause.subordinate(i + 1))))
      }
      subordinates.append(
        schedule(SubtypingConstraint(l.output, r.output, because: c.cause.subordinate(0))))
      return .product(subordinates, failureToSolve(c))

    case (let l as SumType, _ as SumType):
      // If both types are sums, all elements in `L` must be contained in `R`.
      var subordinates: [ConstraintOrigin] = []
      for (i, e) in l.elements.enumerated() {
        subordinates.append(
          schedule(SubtypingConstraint(e, goal.right, because: c.cause.subordinate(i))))
      }
      return .product(subordinates, failureToSolve(c))

    case (_, let r as SumType):
      // If `R` is a sum type and `L` isn't, then `L` must be contained in `R`.
      if r.elements.contains(where: { checker.relations.areEquivalent(goal.left, $0) }) {
        return .success
      }

      // Postpone the constraint if either `L` or `R` contains variables. Otherwise, `L` is not
      // subtype of `R`.
      if goal.left[.hasVariable] || goal.right[.hasVariable] {
        postpone(goal)
        return nil
      } else {
        return .failure(failureToSolve(c))
      }

    default:
      if goal.isStrict {
        return .failure(failureToSolve(c))
      } else {
        return unify(goal.left, goal.right, querying: checker.relations)
          ? .success
          : .failure(failureToSolve(c))
      }
    }
  }

  /// Returns a failure to solve `c`.
  private mutating func failureToSolve(_ c: SubtypingConstraint) -> DiagnoseFailure {
    { (m, _) in
      let (l, r) = (m.reify(c.left), m.reify(c.right))
      switch c.cause.kind {
      case .initializationWithHint:
        return .error(cannotInitialize: l, with: r, at: c.cause.site)
      case .initializationWithPattern:
        return .error(l, doesNotMatch: r, at: c.cause.site)
      default:
        if c.isStrict {
          return .error(l, isNotStrictSubtypeOf: r, at: c.cause.site)
        } else {
          return .error(l, isNotSubtypeOf: r, at: c.cause.site)
        }
      }
    }
  }

  /// Returns either `.success` if instances of `c.left` can be passed to a parameter `c.right`,
  /// `.failure` if they can't, or `nil` if neither of these outcomes can be determined yet.
  private mutating func solve(
    parameter c: ParameterConstraint,
    using checker: inout TypeChecker
  ) -> Outcome? {
    let goal = c.modifyingTypes({ typeAssumptions[$0] })
    if checker.relations.areEquivalent(goal.left, goal.right) {
      return .success
    }

    switch goal.right.base {
    case is TypeVariable:
      // Postpone the constraint until we can infer the parameter passing convention of `R`.
      postpone(goal)
      return nil

    case let p as ParameterType:
      // Either `L` is equal to the bare type of `R`, or it's a. Note: the equality requirement for
      // arguments passed mutably is verified after type inference.
      let s = schedule(SubtypingConstraint(goal.left, p.bareType, because: c.cause.subordinate(0)))
      return .product([s], { (m, r) in
        .error(cannotPass: m.reify(c.left), toParameter: m.reify(c.right), at: c.cause.site)
      })

    default:
      return .failure { (m, _) in
        .error(invalidParameterType: m.reify(c.right), at: c.cause.site)
      }
    }
  }

  /// Returns either `.success` if `c.subject` has a member `c.memberName` of type `c.memberType`,
  /// `.failure` if it doesn't, `.product` if `c` must be broken down to smaller constraints, or
  /// `nil` if neither of these outcomes can be determined yet.
  private mutating func solve(
    member c: MemberConstraint,
    using checker: inout TypeChecker
  ) -> Outcome? {
    let goal = c.modifyingTypes({ typeAssumptions[$0] })

    if goal.subject.base is TypeVariable {
      postpone(goal)
      return nil
    }

    // Generate the list of candidates.
    let matches = checker.lookup(goal.memberName.stem, memberOf: goal.subject, in: scope)
      .compactMap({ checker.decl(in: $0, named: goal.memberName) })
    let candidates = matches.compactMap({ (match) -> OverloadConstraint.Candidate? in
      // Realize the type of the declaration and skip it if that fails.
      let matchType = checker.realize(decl: match)
      if matchType.isError { return nil }

      // TODO: Handle bound generic typess

      return OverloadConstraint.Candidate(
        reference: .member(match),
        type: matchType,
        constraints: [],
        penalties: checker.program.isRequirement(match) ? 1 : 0)
    })

    // Fail if we couldn't find any candidate.
    if candidates.isEmpty {
      return .failure { (m, _) in
        .error(undefinedName: c.memberName, in: m.reify(c.memberType), at: c.cause.site)
      }
    }

    // If there's only one candidate, solve an equality constraint direcly.
    if let pick = candidates.uniqueElement {
      assert(pick.constraints.isEmpty, "not implemented")
      guard unify(pick.type, goal.memberType, querying: checker.relations) else {
        return .failure { (m, _) in
          let (l, r) = (m.reify(pick.type), m.reify(c.memberType))
          return .error(type: l, incompatibleWith: r, at: c.cause.site)
        }
      }

      log("- assume: \"\(goal.memberExpr) &> \(pick.reference)\"")
      bindingAssumptions[goal.memberExpr] = pick.reference
      return .success
    }

    // If there are several candidates, create a overload constraint.
    let s = schedule(
      OverloadConstraint(
        goal.memberExpr, withType: goal.memberType, refersToOneOf: candidates,
        because: c.cause.subordinate(0)))
    return .product([s], { (m, r) in r[s]!.dianoseFailure!(m, r) })
  }

  /// Returns either `.success` if `c.callee` is a callable type with parameters `c.parameters`
  /// and return type `c.returnType`, `.failure` if it doesn't, `.product` if `c` must be broken
  /// down to smaller constraints, or `nil` if neither of these outcomes can be determined yet.
  private mutating func solve(
    functionCall c: FunctionCallConstraint,
    using checker: inout TypeChecker
  ) -> Outcome? {
    let goal = c.modifyingTypes({ typeAssumptions[$0] })

    if goal.calleeType.base is TypeVariable {
      postpone(goal)
      return nil
    }

    guard let callee = goal.calleeType.base as? CallableType else {
      return .failure { (m, _) in
        .error(nonCallableType: m.reify(c.calleeType), at: c.cause.site)
      }
    }

    // Make sure `F` structurally matches the given parameter list.
    if goal.labels.count != callee.labels.count {
      return .failure { (m, _) in
        .error(incompatibleParameterCountAt: c.cause.site)
      }
    } else if !goal.labels.elementsEqual(callee.labels) {
      return .failure { (m, _) in
        .error(labels: c.labels, incompatibleWith: callee.labels, at: c.cause.site)
      }
    }

    // Break down the constraint.
    var subordinates: [ConstraintOrigin] = []
    for i in 0 ..< callee.inputs.count {
      let (a, b) = (callee.inputs[i].type, goal.parameters[i].type)
      subordinates.append(
        schedule(EqualityConstraint(a, b, because: c.cause.subordinate(i + 1))))
    }
    subordinates.append(
      schedule(
        EqualityConstraint(callee.output, goal.returnType, because: c.cause.subordinate(0))))
    return .product(subordinates, { (m, _) in
      .error(function: m.reify(c.calleeType), notCallableWith: c.parameters, at: c.cause.site)
    })
  }

  /// Attempts to solve the remaining constraints for each individual choice in `disjunction` and
  /// returns the best solution.
  private mutating func solve(
    disjunction constraint: DisjunctionConstraint,
    using checker: inout TypeChecker
  ) -> Solution? {
    let results = explore(
      constraint.choices,
      using: &checker,
      configuringSubSolversWith: { (solver, choice) in
        solver.penalties += choice.penalties
        for c in choice.constraints {
          solver.insert(fresh: c)
        }
      })

    if let pick = results.uniqueElement?.solution {
      return pick
    } else if results.isEmpty {
      return nil
    }

    return formAmbiguousSolution(
      results,
      diagnosedBy: .error(ambiguousDisjunctionAt: constraint.cause.site))
  }

  /// Attempts to solve the remaining constraints with each individual choice in `overload` and
  /// returns the best solution.
  private mutating func solve(
    overload constraint: OverloadConstraint,
    using checker: inout TypeChecker
  ) -> Solution? {
    let results = explore(
      constraint.choices,
      using: &checker,
      configuringSubSolversWith: { (solver, choice) in
        solver.penalties += choice.penalties
        solver.bindingAssumptions[constraint.overloadedExpr] = choice.reference
        for c in choice.constraints {
          solver.insert(fresh: c)
        }
      })

    if let pick = results.uniqueElement?.solution {
      return pick
    } else if results.isEmpty {
      return nil
    }

    return formAmbiguousSolution(
      results,
      diagnosedBy: .error(
        ambiguousUse: constraint.overloadedExpr,
        in: checker.ast,
        candidates: results.compactMap(\.choice.reference.decl)))
  }

  /// Solves the remaining constraint with each given choice and returns the best solutions.
  private mutating func explore<Choices: Collection>(
    _ choices: Choices,
    using checker: inout TypeChecker,
    configuringSubSolversWith configureSubSolver: (inout Self, Choices.Element) -> Void
  ) -> [Exploration<Choices.Element>] where Choices.Element: Choice {
    log("- fork:")
    indentation += 1
    defer { indentation -= 1 }

    /// The results of the exploration.
    var results: [Exploration<Choices.Element>] = []

    for choice in choices {
      // Don't bother if there's no chance to find a better solution.
      var underestimatedChoiceScore = score()
      underestimatedChoiceScore.penalties += choice.penalties
      if underestimatedChoiceScore > bestScore {
        log("- skip: \"\(choice)\"")
        continue
      }

      log("- pick: \"\(choice)\"")
      indentation += 1
      defer { indentation -= 1 }

      // Explore the result of this choice.
      var subSolver = self
      configureSubSolver(&subSolver, choice)
      guard let newSolution = subSolver.solveConstraints(&checker) else { continue }

      // Insert the new result.
      insert((choice, newSolution), into: &results, using: &checker)
    }

    return results
  }

  /// Inserts `newResult` into `bestResults` if its solution is better than or incomparable to any
  /// of sthe latter's elements.
  private mutating func insert<T>(
    _ newResult: Exploration<T>,
    into bestResults: inout [Exploration<T>],
    using checker: inout TypeChecker
  ) {
    // Rank solutions based on the name bindings they make. `s1` refines `s2` iff it has a better
    // score than `s2` or if it has the same score but makes at least one more specific binding
    // than `s2` and no binding less specific than `s2`.
    if newResult.solution.score > bestScore { return }

    // Fast path: if the new solution has a better score, discard all others.
    if bestResults.isEmpty || (newResult.solution.score < bestScore) {
      bestScore = newResult.solution.score
      bestResults = [newResult]
      return
    }

    // Slow path: inspect how the new solution compares with the ones we have.
    var shouldInsert = false
    var i = 0
    while i < bestResults.count {
      // Check if the new solution binds name expressions to more specialized declarations.
      let comparison = checker.compareSolutionBindings(
        newResult.solution, bestResults[0].solution, scope: scope)
      switch comparison {
      case .comparable(.equal):
        // The new solution is equal; discard it.
        return
      case .comparable(.coarser):
        // The new solution is coarser; discard it unless it's better than another one.
        i += 1
      case .comparable(.finer):
        // The new solution is finer; keep it and discard the old one.
        bestResults.remove(at: i)
        shouldInsert = true
      case .incomparable:
        // The new solution is incomparable; keep it.
        i += 1
        shouldInsert = true
      }
    }

    if shouldInsert {
      bestResults.append(newResult)
    }
  }

  /// Schedules `constraint` to be solved in the future and returns its origin.
  @discardableResult
  private mutating func schedule(_ constraint: Constraint) -> ConstraintOrigin {
    log("- schedule: \"\(constraint)\"")
    insert(fresh: constraint)
    return constraint.cause
  }

  /// Schedules `constraint` to be solved only once the solver has inferred more information about
  /// at least one of its type variables.
  ///
  /// - Requires: `constraint` must involve type variables.
  private mutating func postpone(_ constraint: Constraint) {
    log("- postpone: \"\(constraint)\"")
    insert(stale: constraint)
  }

  /// Inserts `constraint` into the fresh set.
  private mutating func insert(fresh constraint: Constraint) {
    fresh.append(constraint)
  }

  /// Inserts `constraint` into the stale set.
  private mutating func insert(stale constraint: Constraint) {
    stale.append(constraint)
  }

  /// Unifies `lhs` with `rhs` using `relations` to check for equivalence, returning `true` if
  /// unification succeeded.
  ///
  /// Type unification consists of finding substitutions that makes `lhs` and `rhs` equal. The
  /// algorithm recursively visits both types in lockstep, updating `self.typeAssumptions` every
  /// time either side is a type variable for which no substitution has been made yet.
  private mutating func unify(
    _ lhs: AnyType, _ rhs: AnyType,
    querying relations: TypeRelations
  ) -> Bool {
    let (lhs, rhs) = (typeAssumptions[lhs], typeAssumptions[rhs])
    if relations.areEquivalent(lhs, rhs) { return true }

    switch (lhs.base, rhs.base) {
    case (let v as TypeVariable, _):
      assume(v, equals: rhs)
      return true

    case (_, let v as TypeVariable):
      assume(v, equals: lhs)
      return true

    case (let l as TupleType, let r as TupleType):
      if !l.labels.elementsEqual(r.labels) {
        return false
      }

      var result = true
      for (a, b) in zip(l.elements, r.elements) {
        result = unify(a.type, b.type, querying: relations) && result
      }
      return result

    case (let l as LambdaType, let r as LambdaType):
      if !l.labels.elementsEqual(r.labels) {
        return false
      }

      var result = true
      for (a, b) in zip(l.inputs, r.inputs) {
        result = unify(a.type, b.type, querying: relations) && result
      }
      result = unify(l.output, r.output, querying: relations) && result
      result = unify(l.environment, r.environment, querying: relations) && result
      return result

    case (let l as MethodType, let r as MethodType):
      if !l.labels.elementsEqual(r.labels) {
        return false
      }
      if l.capabilities != r.capabilities {
        return false
      }

      var result = true
      for (a, b) in zip(l.inputs, r.inputs) {
        result = unify(a.type, b.type, querying: relations) && result
      }
      result = unify(l.output, r.output, querying: relations) && result
      result = unify(l.receiver, r.receiver, querying: relations) && result
      return result

    case (let l as ParameterType, let r as ParameterType):
      if l.access != r.access {
        return false
      }
      return unify(l.bareType, r.bareType, querying: relations)

    case (let l as RemoteType, let r as RemoteType):
      if l.access != r.access {
        return false
      }
      return unify(l.bareType, r.bareType, querying: relations)

    default:
      return false
    }
  }

  /// Extends the type substution table to map `tau` to `substitute`.
  private mutating func assume(_ tau: TypeVariable, equals substitute: AnyType) {
    log("- assume: \"\(tau) = \(substitute)\"")
    typeAssumptions.assign(substitute, to: tau)

    // Refresh stale constraints.
    for i in (0 ..< stale.count).reversed() {
      var changed = false
      let updated = stale[i].modifyingTypes({ (type) in
        let u = typeAssumptions.reify(type, withVariables: .keep)
        changed = changed || (type != u)
        return u
      })

      if changed {
        log("- refresh \(stale[i])")
        stale.remove(at: i)
        fresh.append(updated)
      }
    }
  }

  /// Transforms the stale literal constraints to equality constraints.
  private mutating func refreshLiteralConstraints() {
    for i in (0 ..< stale.count).reversed() {
      if let c = stale[i] as? LiteralConstraint {
        log("- refresh \(stale[i])")
        fresh.append(EqualityConstraint(c.subject, c.defaultSubject, because: c.cause))
        stale.remove(at: i)
      }
    }
  }

  /// Creates a solution from the current state.
  private mutating func formSolution() -> Solution {
    assert(fresh.isEmpty)
    let m = typeAssumptions.optimized()

    var d = DiagnosticSet(stale.map(Diagnostic.error(staleConstraint:)))
    for (k, v) in results where iFailureRoot(k) {
      d.insert(v.dianoseFailure!(m, results))
    }

    return Solution(
      substitutions: m, bindings: bindingAssumptions, penalties: penalties, diagnostics: d)
  }

  /// Creates an ambiguous solution.
  private func formAmbiguousSolution<T>(
    _ results: [Exploration<T>],
    diagnosedBy d: Diagnostic
  ) -> Solution {
    var s = results.dropFirst().reduce(into: results[0].solution) { (s, r) in
      s.merge(r.solution)
    }
    s.incorporate(d)
    return s
  }

  /// Logs a line of text in the standard output.
  private func log(_ line: @autoclosure () -> String) {
    if !isLoggingEnabled { return }
    print(String(repeating: "  ", count: indentation) + line())
  }

  /// Logs the current state of `self` in the standard output.
  private func logState() {
    if !isLoggingEnabled { return }
    log("fresh:")
    for c in fresh {
      log("- - \"\(c)\"")
      log("  - \"\(c.cause)\"")
    }
    log("stale:")
    for c in stale {
      log("- - \"\(c)\"")
      log("  - \"\(c.cause)\"")
    }
  }

}

/// A type representing a choice during constraint solving.
private protocol Choice {

  /// The set of constraints associated with this choice.
  var constraints: ConstraintSet { get }

  /// The penalties associated with this choice.
  var penalties: Int { get }

}

extension DisjunctionConstraint.Choice: Choice {}

extension OverloadConstraint.Candidate: Choice {}

extension TypeChecker {

  fileprivate enum SolutionBingindsComparison {

    enum Ranking: Int8, Comparable {

      case finer = -1

      case equal = 0

      case coarser = 1

      static func < (l: Self, r: Self) -> Bool {
        l.rawValue < r.rawValue
      }

    }

    case incomparable

    case comparable(Ranking)

  }

  fileprivate mutating func compareSolutionBindings(
    _ lhs: Solution,
    _ rhs: Solution,
    scope: AnyScopeID
  ) -> SolutionBingindsComparison {
    var ranking: SolutionBingindsComparison.Ranking = .equal
    var namesInCommon = 0

    for (n, lhsDeclRef) in lhs.bindingAssumptions {
      guard let rhsDeclRef = rhs.bindingAssumptions[n] else { continue }
      namesInCommon += 1

      // Nothing to do if both functions have the binding.
      if lhsDeclRef == rhsDeclRef { continue }
      let lhs = declTypes[lhsDeclRef.decl!]!
      let rhs = declTypes[rhsDeclRef.decl!]!

      switch (lhs.base, rhs.base) {
      case (let l as CallableType, let r as CallableType):
        // Candidates must accept the same number of arguments and have the same labels.
        guard
          l.inputs.count == r.inputs.count,
          l.inputs.elementsEqual(r.inputs, by: { $0.label == $1.label })
        else { return .incomparable }

        // Rank the candidates.
        let lRefinesR = refines(lhs, rhs, in: scope, anchoringConstraintsAt: program.ast[n].site)
        let rRefinesL = refines(rhs, lhs, in: scope, anchoringConstraintsAt: program.ast[n].site)
        switch (lRefinesR, rRefinesL) {
        case (true, false):
          if ranking > .equal { return .incomparable }
          ranking = .finer
        case (false, true):
          if ranking < .equal { return .incomparable }
          ranking = .coarser
        default:
          return .incomparable
        }

      default:
        return .incomparable
      }
    }

    if lhs.bindingAssumptions.count < rhs.bindingAssumptions.count {
      if namesInCommon == lhs.bindingAssumptions.count {
        return ranking >= .equal ? .comparable(.coarser) : .incomparable
      } else {
        return .incomparable
      }
    }

    if lhs.bindingAssumptions.count > rhs.bindingAssumptions.count {
      if namesInCommon == rhs.bindingAssumptions.count {
        return ranking <= .equal ? .comparable(.finer) : .incomparable
      } else {
        return .incomparable
      }
    }

    return namesInCommon == lhs.bindingAssumptions.count ? .comparable(ranking) : .incomparable
  }

  fileprivate mutating func refines(
    _ l: AnyType,
    _ r: AnyType,
    in scope: AnyScopeID,
    anchoringConstraintsAt site: SourceRange
  ) -> Bool {
    // Skolemize the left operand.
    let skolemizedLeft = l.skolemized

    // Open the right operand.
    let openedRight = open(type: r)
    var constraints = openedRight.constraints

    // Create pairwise subtyping constraints on the parameters.
    let lhs = skolemizedLeft.base as! CallableType
    let rhs = openedRight.shape.base as! CallableType

    for i in 0 ..< lhs.inputs.count {
      // Ignore the passing conventions.
      guard
        let bareLHS = ParameterType(lhs.inputs[i].type)?.bareType,
        let bareRHS = ParameterType(rhs.inputs[i].type)?.bareType
      else { return false }

      constraints.insert(
        SubtypingConstraint(bareLHS, bareRHS, because: ConstraintOrigin(.binding, at: site)))
    }

    // Solve the constraint system.
    var solver = ConstraintSolver(scope: scope, fresh: constraints, loggingTrace: false)
    return !solver.solution(&self).diagnostics.containsError
  }

}

/// Creates a constraint, suitable for type inference, requiring `subtype` to be a subtype of
/// `supertype`.
///
/// - Warning: For inference purposes, the result of this function must be used in place of a raw
///   `SubtypingConstraint` or the type checker will get stuck.
private func inferenceConstraint(
  _ subtype: AnyType,
  isSubtypeOf supertype: AnyType,
  because cause: ConstraintOrigin
) -> Constraint {
  // If there aren't any type variable in neither `subtype` nor `supertype`, there's nothing to
  // infer and we can return a regular subtyping constraints.
  if !subtype[.hasVariable] && !supertype[.hasVariable] {
    return SubtypingConstraint(subtype, supertype, because: cause)
  }

  // In other cases, we'll need two explorations. The first will unify `subtype` and `supertype`
  // and the other will try to infer them from the other constraints in the system.
  let alternative: Constraint
  if supertype.isLeaf {
    // If the supertype is a leaf, the subtype can only the same type or `Never`.
    alternative = EqualityConstraint(subtype, .never, because: cause)
  } else {
    // Otherwise, the subtype can be any type upper-bounded by the supertype.
    alternative = SubtypingConstraint(subtype, supertype, strictly: true, because: cause)
  }

  return DisjunctionConstraint(
    choices: [
      .init(constraints: [EqualityConstraint(subtype, supertype, because: cause)], penalties: 0),
      .init(constraints: [alternative], penalties: 1),
    ],
    because: cause)
}

typealias ThreeValuedBit = Bool?

extension ThreeValuedBit {

  static func && (l: Self, r: Self) -> Self {
    if let a = l {
      return a ? r : false
    }
    if let b = r {
      return b ? l : false
    }
    return nil
  }

}
