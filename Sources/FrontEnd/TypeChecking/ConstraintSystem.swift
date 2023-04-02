import Core
import Utils

/// A collection of constraints over a set of open type variables and a set of unresolved name
/// expressions.
struct ConstraintSystem {

  /// The identity of a gaol in an instance of `ConstraintSystem`.
  private typealias GoalIdentity = Int

  /// A map from goal to its outcome.
  private typealias OutcomeMap = [Outcome?]

  /// A closure reporting the diagnostics of a goal's failure into `d`, using `m` to reify types
  /// and reading the outcome of other goals from `o`.
  private typealias DiagnoseFailure = (
    _ d: inout DiagnosticSet,
    _ m: SubstitutionMap,
    _ o: OutcomeMap
  ) -> Void

  /// The outcome of a goal.
  private enum Outcome {

    /// The goal was solved.
    ///
    /// Information inferred from the goal has been stored in the solver's state.
    case success

    /// The goal was unsatisfiable.
    ///
    /// The goal was in conflict with the information inferred by the solver. The payload is a
    /// closure that generates a diagnostic of the conflict.
    case failure(DiagnoseFailure)

    /// The goal was broken into subordinate goal.
    ///
    /// The payload is a non-empty array of subordinate goals along with a closure that's used to
    /// generate a diagnostic in case one of the subordinate goals are unsatisfiable.
    case product([GoalIdentity], DiagnoseFailure)

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

  /// The scope in which the goals are solved.
  private let scope: AnyScopeID

  /// The goals in the system.
  private var goals: [Constraint] = []

  /// A map from goal its outcome.
  ///
  /// - Invariant: This array has the same length as `this.goals`.
  private var outcomes: OutcomeMap = []

  /// The fresh goals to solve.
  private var fresh: [GoalIdentity] = []

  /// The goals that are currently stale.
  private var stale: [GoalIdentity] = []

  /// A map from open type variable to its assignment.
  ///
  /// This map is monotonically extended during constraint solving to assign a type to each open
  /// variable in the constraint system. A system is complete if it can be used to derive a
  /// complete substitution map w.r.t. its open type variables.
  private var typeAssumptions = SubstitutionMap()

  /// A map from name expression to its referred declaration.
  ///
  /// This map is monotonically extended during constraint solving to assign a declaration to each
  /// unresolved name expression in the constraint system. A system is complete if it can be used
  /// to derive a complete name binding map w.r.t. its unresolved name expressions.
  private var bindingAssumptions: [NameExpr.ID: DeclRef] = [:]

  /// The penalties associated with the constraint system.
  ///
  /// This value serves to prune explorations that cannot produce a better solution than one
  /// already computed.
  private var penalties: Int = 0

  /// The score of the best solution computed so far.
  private var bestScore = Solution.Score.worst

  /// Indicates whether this instance should log a trace.
  private let isLoggingEnabled: Bool

  /// The current indentation level for logging messages.
  private var indentation = 0

  /// Creates an instance with given `constraints` defined in `scope`.
  init<S: Sequence<Constraint>>(
    _ constraints: S, in scope: AnyScopeID, loggingTrace isLoggingEnabled: Bool
  ) {
    self.scope = scope
    self.isLoggingEnabled = isLoggingEnabled
    _ = insert(fresh: constraints)
  }

  /// Solves this instance using `checker` to query type relations and resolve names, returning the
  /// best solution.
  mutating func solution(_ checker: inout TypeChecker) -> Solution {
    betterSolution(&checker)!
  }

  /// Solves this instance using `checker` to query type relations and resolve names, returning the
  /// best solution or `nil` if no solution with a score better than `self.bestScore` can be found.
  private mutating func betterSolution(_ checker: inout TypeChecker) -> Solution? {
    logState()
    log("steps:")

    while let g = fresh.popLast() {
      // Make sure the current solution is still worth exploring.
      if score() > bestScore {
        log("- abort")
        return nil
      }

      goals[g].modifyTypes({ typeAssumptions[$0] })
      log("- solve: \"\(goals[g])\"")
      indentation += 1
      defer { indentation -= 1 }
      log("actions:")

      switch goals[g] {
      case is ConformanceConstraint:
        setOutcome(solve(conformance: g, using: &checker), for: g)
      case is EqualityConstraint:
        setOutcome(solve(equality: g, using: &checker), for: g)
      case is SubtypingConstraint:
        setOutcome(solve(subtyping: g, using: &checker), for: g)
      case is ParameterConstraint:
        setOutcome(solve(parameter: g, using: &checker), for: g)
      case is MemberConstraint:
        setOutcome(solve(member: g, using: &checker), for: g)
      case is TupleMemberConstraint:
        setOutcome(solve(tupleMember: g, using: &checker), for: g)
      case is FunctionCallConstraint:
        setOutcome(solve(functionCall: g, using: &checker), for: g)
      case is MergingConstraint:
        setOutcome(solve(merging: g, using: &checker), for: g)
      case is DisjunctionConstraint:
        return solve(disjunction: g, using: &checker)
      case is OverloadConstraint:
        return solve(overload: g, using: &checker)
      default:
        unreachable()
      }
    }

    return formSolution()
  }

  /// Returns the currently known cost of the solution being computed.
  ///
  /// The cost of a solution increases monotonically when a constraint is eliminated.
  private func score() -> Solution.Score {
    .init(
      errorCount: goals.indices.elementCount(where: isFailureRoot),
      penalties: penalties)
  }

  /// Returns `true` iff the solving `g` failed and `g` isn't subordinate.
  private func isFailureRoot(_ g: GoalIdentity) -> Bool {
    (goals[g].origin.parent == nil) && (succeeded(g) == false)
  }

  /// Returns whether the solving `g` succeeded or `.none` if outcome has been computed yet.
  private func succeeded(_ g: GoalIdentity) -> ThreeValuedBit {
    switch outcomes[g] {
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

  /// Records the outcome `value` for the goal `key`.
  private mutating func setOutcome(_ value: Outcome?, for key: GoalIdentity) {
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

    assert(outcomes[key] == nil)
    outcomes[key] = value
  }

  /// Creates a solution from the current state.
  private mutating func formSolution() -> Solution {
    assert(fresh.isEmpty)
    assert(outcomes.enumerated().allSatisfy({ (i, o) in (o != nil) || stale.contains(i) }))

    for g in stale {
      let c = goals[g]
      setOutcome(.failure({ (d, _, _) in d.insert(.error(staleConstraint: c)) }), for: g)
    }

    let m = typeAssumptions.optimized()
    var d = DiagnosticSet()
    for (k, v) in zip(goals.indices, outcomes) where isFailureRoot(k) {
      v!.dianoseFailure!(&d, m, outcomes)
    }

    return Solution(
      substitutions: m, bindings: bindingAssumptions, penalties: penalties, diagnostics: d)
  }

  /// Creates an ambiguous solution.
  private func formAmbiguousSolution<T>(
    _ results: Explorations<T>,
    diagnosedBy d: Diagnostic
  ) -> Solution {
    var s = results.elements.dropFirst().reduce(into: results.elements[0].solution) { (s, r) in
      s.merge(r.solution)
    }
    s.incorporate(d)
    return s
  }

  /// Returns either `.success` if `g.subject` conforms to `g.traits`, `.failure` if it doesn't, or
  /// `nil` if neither of these outcomes can be be determined yet.
  private mutating func solve(
    conformance g: GoalIdentity,
    using checker: inout TypeChecker
  ) -> Outcome? {
    let goal = goals[g] as! ConformanceConstraint

    var missingTraits: Set<TraitType>
    switch goal.subject.base {
    case is TypeVariable:
      postpone(g)
      return nil

    case is BuiltinType:
      // Built-in types are `Sinkable`.
      missingTraits = goal.traits.subtracting(
        [checker.ast.coreTrait(named: "Sinkable")!])

    default:
      missingTraits = goal.traits.subtracting(
        checker.conformedTraits(of: goal.subject, in: scope))
    }

    if missingTraits.isEmpty {
      return .success
    } else {
      return .failure { (d, m, _) in
        let t = m.reify(goal.subject)
        d.formUnion(missingTraits.map({ .error(t, doesNotConformTo: $0, at: goal.origin.site) }))
      }
    }
  }

  /// Returns eiteher `.success` if `g.left` is unifiable with `g.right` or `.failure` otherwise.
  private mutating func solve(
    equality g: GoalIdentity,
    using checker: inout TypeChecker
  ) -> Outcome {
    let goal = goals[g] as! EqualityConstraint
    if unify(goal.left, goal.right, querying: checker.relations) {
      return .success
    } else {
      return .failure { (d, m, _) in
        let (l, r) = (m.reify(goal.left), m.reify(goal.right))
        d.insert(.error(type: l, incompatibleWith: r, at: goal.origin.site))
      }
    }
  }

  /// Returns either `.success` if `g.left` is (strictly) subtype of `g.right`, `.failure` if it
  /// isn't, `.product` if `g` must be broken down to smaller goals, or `nil` if that can't be
  /// determined yet.
  private mutating func solve(
    subtyping g: GoalIdentity,
    using checker: inout TypeChecker
  ) -> Outcome? {
    let goal = goals[g] as! SubtypingConstraint
    if checker.relations.areEquivalent(goal.left, goal.right) {
      return goal.isStrict ? .failure(failureToSolve(goal)) : .success
    }

    switch (goal.left.base, goal.right.base) {
    case (_, _ as TypeVariable):
      // The type variable is above a more concrete type. We should compute the "join" of all types
      // to which `L` is coercible and that are below `R`, but that set is unbounded. We have no
      // choice but to postpone the goal.
      if goal.isStrict {
        postpone(g)
        return nil
      } else {
        let s = schedule(
          inferenceConstraint(goal.left, isSubtypeOf: goal.right, origin: goal.origin))
        return delegate(to: [s])
      }

    case (_ as TypeVariable, _):
      // The type variable is below a more concrete type. We should compute the "meet" of all types
      // coercible to `R` and that are above `L`, but that set is unbounded unless `R` is a leaf.
      // If it isn't, we have no choice but to postpone the goal.
      if goal.right.isLeaf {
        return unify(goal.left, goal.right, querying: checker.relations)
          ? .success
          : .failure(failureToSolve(goal))
      } else if goal.isStrict {
        postpone(g)
        return nil
      } else {
        let s = schedule(
          inferenceConstraint(goal.left, isSubtypeOf: goal.right, origin: goal.origin))
        return delegate(to: [s])
      }

    case (_, _ as ExistentialType):
      // All types conform to any.
      if goal.right == .any { return .success }
      fatalError("not implemented")

    case (let l as LambdaType, let r as LambdaType):
      if !l.labels.elementsEqual(r.labels) {
        return .failure(failureToSolve(goal))
      }
      if !unify(l.environment, r.environment, querying: checker.relations) {
        return .failure(failureToSolve(goal))
      }

      // Parameters are contravariant; return types are covariant.
      var subordinates: [GoalIdentity] = []
      for (a, b) in zip(l.inputs, r.inputs) {
        subordinates.append(
          schedule(SubtypingConstraint(b.type, a.type, origin: goal.origin.subordinate())))
      }
      subordinates.append(
        schedule(SubtypingConstraint(l.output, r.output, origin: goal.origin.subordinate())))
      return .product(subordinates, failureToSolve(goal))

    case (let l as SumType, _ as SumType):
      // If both types are sums, all elements in `L` must be contained in `R`.
      var subordinates: [GoalIdentity] = []
      for e in l.elements {
        subordinates.append(
          schedule(SubtypingConstraint(e, goal.right, origin: goal.origin.subordinate())))
      }
      return .product(subordinates, failureToSolve(goal))

    case (_, let r as SumType):
      // If `R` is a sum type and `L` isn't, then `L` must be contained in `R`.
      if r.elements.contains(where: { checker.relations.areEquivalent(goal.left, $0) }) {
        return .success
      }

      // Postpone the goal if either `L` or `R` contains variables. Otherwise, `L` is not subtype
      // of `R`.
      if goal.left[.hasVariable] || goal.right[.hasVariable] {
        postpone(g)
        return nil
      } else {
        return .failure(failureToSolve(goal))
      }

    default:
      if goal.isStrict {
        return .failure(failureToSolve(goal))
      } else {
        return unify(goal.left, goal.right, querying: checker.relations)
          ? .success
          : .failure(failureToSolve(goal))
      }
    }
  }

  /// Returns a clousre diagnosing a failure to solve `g`.
  private mutating func failureToSolve(_ g: SubtypingConstraint) -> DiagnoseFailure {
    { (d, m, _) in
      let (l, r) = (m.reify(g.left), m.reify(g.right))
      switch g.origin.kind {
      case .initializationWithHint:
        d.insert(.error(cannotInitialize: l, with: r, at: g.origin.site))
      case .initializationWithPattern:
        d.insert(.error(l, doesNotMatch: r, at: g.origin.site))
      default:
        if g.isStrict {
          d.insert(.error(l, isNotStrictSubtypeOf: r, at: g.origin.site))
        } else {
          d.insert(.error(l, isNotSubtypeOf: r, at: g.origin.site))
        }
      }
    }
  }

  /// Returns either `.success` if instances of `g.left` can be passed to a parameter `g.right`,
  /// `.failure` if they can't, or `nil` if neither of these outcomes can be determined yet.
  private mutating func solve(
    parameter g: GoalIdentity,
    using checker: inout TypeChecker
  ) -> Outcome? {
    let goal = goals[g] as! ParameterConstraint
    if checker.relations.areEquivalent(goal.left, goal.right) {
      return .success
    }

    switch goal.right.base {
    case is TypeVariable:
      // Postpone the goal until we can infer the parameter passing convention of `R`.
      postpone(g)
      return nil

    case let p as ParameterType:
      // Either `L` is equal to the bare type of `R`, or it's a. Note: the equality requirement for
      // arguments passed mutably is verified after type inference.
      let s = schedule(
        SubtypingConstraint(goal.left, p.bareType, origin: goal.origin.subordinate()))
      return .product([s]) { (d, m, r) in
        let (l, r) = (m.reify(goal.left), m.reify(goal.right))
        d.insert(.error(cannotPass: l, toParameter: r, at: goal.origin.site))
      }

    default:
      return .failure { (d, m, _) in
        d.insert(.error(invalidParameterType: m.reify(goal.right), at: goal.origin.site))
      }
    }
  }

  /// Returns either `.success` if `g.subject` has a member `g.memberName` of type `g.memberType`,
  /// `.failure` if it doesn't, `.product` if `g` must be broken down to smaller goals, or `nil` if
  /// if neither of these outcomes can be determined yet.
  private mutating func solve(
    member g: GoalIdentity,
    using checker: inout TypeChecker
  ) -> Outcome? {
    let goal = goals[g] as! MemberConstraint

    if goal.subject.base is TypeVariable {
      postpone(g)
      return nil
    }

    // Generate the list of candidates.
    let matches = checker.lookup(goal.memberName.stem, memberOf: goal.subject, exposedTo: scope)
      .compactMap({ checker.decl(in: $0, named: goal.memberName) })
    let candidates = matches.compactMap({ (match) -> OverloadConstraint.Predicate? in
      // Realize the type of the declaration and skip it if that fails.
      let matchType = checker.realize(decl: match)
      if matchType.isError { return nil }

      // TODO: Handle bound generic typess

      return OverloadConstraint.Predicate(
        reference: .member(match),
        type: matchType,
        constraints: [],
        penalties: checker.program.isRequirement(match) ? 1 : 0)
    })

    // Fail if we couldn't find any candidate.
    if candidates.isEmpty {
      return .failure { (d, m, _) in
        let t = m.reify(goal.memberType)
        d.insert(.error(undefinedName: goal.memberName, in: t, at: goal.origin.site))
      }
    }

    // If there's only one candidate, solve an equality constraint direcly.
    if let pick = candidates.uniqueElement {
      assert(pick.constraints.isEmpty, "not implemented")
      guard unify(pick.type, goal.memberType, querying: checker.relations) else {
        return .failure { (d, m, _) in
          let (l, r) = (m.reify(pick.type), m.reify(goal.memberType))
          d.insert(.error(type: l, incompatibleWith: r, at: goal.origin.site))
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
        origin: goal.origin.subordinate()))
    return delegate(to: [s])
  }

  /// Returns either `.success` if `g.subject` is a tuple type whose `g.elementIndex`-th element
  /// has type `g.elementType`, `.failure` if it doesn't, `.product` if `g` must be broken down to
  /// smaller goals, or `nil` if if neither of these outcomes can be determined yet.
  private mutating func solve(
    tupleMember g: GoalIdentity,
    using checker: inout TypeChecker
  ) -> Outcome? {
    let goal = goals[g] as! TupleMemberConstraint

    switch goal.subject.base {
    case is TypeVariable:
      postpone(g)
      return nil

    case let s as TupleType:
      guard goal.elementIndex < s.elements.count else { break }

      let t = s.elements[goal.elementIndex].type
      if unify(t, goal.elementType, querying: checker.relations) {
        return .success
      } else {
        return .failure { (d, m, _) in
          let (l, r) = (m.reify(t), m.reify(goal.elementType))
          d.insert(.error(type: l, incompatibleWith: r, at: goal.origin.site))
        }
      }

    default:
      // TODO: Handle bound generic typess
      break
    }

    return .failure { (d, m, _) in
      let s = m.reify(goal.subject)
      d.insert(.error(undefinedName: goal.elementIndex, in: s, at: goal.origin.site))
    }
  }

  /// Returns either `.success` if `g.callee` is a callable type with parameters `g.parameters`
  /// and return type `g.returnType`, `.failure` if it doesn't, `.product` if `g` must be broken
  /// down to smaller goals, or `nil` if neither of these outcomes can be determined yet.
  private mutating func solve(
    functionCall g: GoalIdentity,
    using checker: inout TypeChecker
  ) -> Outcome? {
    let goal = goals[g] as! FunctionCallConstraint

    if goal.calleeType.base is TypeVariable {
      postpone(g)
      return nil
    }

    guard let callee = goal.calleeType.base as? CallableType else {
      return .failure { (d, m, _) in
        d.insert(.error(nonCallableType: m.reify(goal.calleeType), at: goal.origin.site))
      }
    }

    // Make sure `F` structurally matches the given parameter list.
    if goal.labels.count != callee.labels.count {
      return .failure { (d, m, _) in
        d.insert(.error(incompatibleParameterCountAt: goal.origin.site))
      }
    } else if !goal.labels.elementsEqual(callee.labels) {
      return .failure { (d, m, _) in
        d.insert(
          .error(labels: goal.labels, incompatibleWith: callee.labels, at: goal.origin.site))
      }
    }

    // Break down the goal.
    var subordinates: [GoalIdentity] = []
    for (a, b) in zip(callee.inputs, goal.parameters) {
      subordinates.append(
        schedule(EqualityConstraint(a.type, b.type, origin: goal.origin.subordinate())))
    }
    subordinates.append(
      schedule(
        EqualityConstraint(callee.output, goal.returnType, origin: goal.origin.subordinate())))
    return delegate(to: subordinates)
  }

  private mutating func solve(
    merging g: GoalIdentity,
    using checker: inout TypeChecker
  ) -> Outcome? {
    let goal = goals[g] as! MergingConstraint
    guard !goal.branches.isEmpty else { return .success }

    var subordinates: [GoalIdentity] = []
    for b in goal.branches {
      subordinates.append(
        schedule(SubtypingConstraint(goal.supertype, b, origin: goal.origin.subordinate())))
    }
    return .product(subordinates) { (d, m, _) in
      let t = goal.branches.map({ m.reify($0) })
      d.insert(.error(conditionalHasMismatchingTypes: t, at: goal.origin.site))
    }
  }

  /// Solves the remaining goals separately for each choice in `g` and returns the best solution.
  private mutating func solve(
    disjunction g: GoalIdentity,
    using checker: inout TypeChecker
  ) -> Solution? {
    let goal = goals[g] as! DisjunctionConstraint

    let results: Explorations<DisjunctionConstraint> = explore(
      g, using: &checker,
      configuringSubSystemWith: { (solver, choice) in
        solver.penalties += choice.penalties
        return solver.insert(fresh: choice.constraints)
      })

    if let pick = results.elements.uniqueElement?.solution {
      return pick
    } else if results.elements.isEmpty {
      return nil
    }

    return formAmbiguousSolution(
      results, diagnosedBy: .error(ambiguousDisjunctionAt: goal.origin.site))
  }

  /// Solves the remaining goals separately for each choice in `g` and returns the best solution.
  private mutating func solve(
    overload g: GoalIdentity,
    using checker: inout TypeChecker
  ) -> Solution? {
    let goal = goals[g] as! OverloadConstraint

    let results: Explorations<OverloadConstraint> = explore(
      g, using: &checker,
      configuringSubSystemWith: { (solver, choice) in
        solver.penalties += choice.penalties
        solver.bindingAssumptions[goal.overloadedExpr] = choice.reference
        return solver.insert(fresh: choice.constraints)
      })

    if let pick = results.elements.uniqueElement?.solution {
      return pick
    } else if results.elements.isEmpty {
      return nil
    }

    return formAmbiguousSolution(
      results,
      diagnosedBy: .error(
        ambiguousUse: goal.overloadedExpr,
        in: checker.ast,
        candidates: results.elements.compactMap(\.choice.reference.decl)))
  }

  /// Solves the remaining goals in `self` exploring each choice in `g` with a separate constraint
  /// systems configured with `configureSubSystem` returns the best solution of each exploration.
  ///
  /// - Parameters:
  ///   - g: the goal whose choices whould be explored; must denote a constraint of type `T`.
  ///   - checker: an instance used to query type relations and resolve names.
  ///   - configureSubSystem: A closure that prepares a constraint system using one of `g`'s
  ///     choices and returns the identities of the goals added to that system.
  private mutating func explore<T: DisjunctiveConstraintProtocol>(
    _ g: GoalIdentity,
    using checker: inout TypeChecker,
    configuringSubSystemWith configureSubSystem: (inout Self, T.Predicate) -> [GoalIdentity]
  ) -> Explorations<T> {
    log("- fork:")
    indentation += 1
    defer { indentation -= 1 }

    var results = Explorations<T>()
    for choice in (goals[g] as! T).choices {
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
      var exploration = self
      let s = configureSubSystem(&exploration, choice)
      exploration.setOutcome(s.isEmpty ? .success : delegate(to: s), for: g)
      guard let newSolution = exploration.betterSolution(&checker) else { continue }

      // Insert the new result.
      results.insert(
        (choice, newSolution),
        rankingSolutionWith: { (a, b) in rank(a, b, querying: &checker) })
    }

    return results
  }

  /// Returns an outcome indicating that a goal has been broken into given `subordinates` and
  /// forwards their diagnostics.
  private func delegate(to s: [GoalIdentity]) -> Outcome {
    .product(s, { (d, m, r) in s.forEach({ r[$0]!.dianoseFailure?(&d, m, r) }) })
  }

  /// Schedules `g` to be solved in the future and returns its identity.
  private mutating func schedule(_ g: Constraint) -> GoalIdentity {
    log("- schedule: \"\(g)\"")
    return insert(fresh: g)
  }

  /// Inserts `g` into the fresh set and returns its identity.
  private mutating func insert(fresh g: Constraint) -> GoalIdentity {
    // Note: It could be worth looking for the index of a goal equal to `g` rather than appending
    // it unconditionally so that we don't solve the same constraint twice. However, efficient
    // lookup would require a set while we need constraint indices to be stable identities. One
    // solution would be to implement `goals` and `outcomes` as persistent data structures.
    let newIdentity = goals.count
    goals.append(g)
    outcomes.append(nil)

    let i = fresh.partitioningIndex(
      at: newIdentity,
      orderedBy: { (a, b) in !goals[a].simpler(than: goals[b]) })
    fresh.insert(newIdentity, at: i)
    return newIdentity
  }

  /// Inserts `batch` into the fresh set.
  private mutating func insert<S: Sequence<Constraint>>(fresh batch: S) -> [GoalIdentity] {
    batch.map { (g) in insert(fresh: g) }
  }

  /// Schedules `g` to be solved only once the solver has inferred more information about at least
  /// one of its type variables.
  ///
  /// - Requires: `g` must involve type variables.
  private mutating func postpone(_ g: GoalIdentity) {
    stale.append(g)
  }

  /// Unifies `lhs` with `rhs` using `relations` to check for equivalence, returning `true` if
  /// unification succeeded.
  ///
  /// Type unification consists of finding substitutions that makes `lhs` and `rhs` equal. The
  /// algorithm recursively visits both types in lockstep, updating `self.typeAssumptions` every
  /// time either side is a type variable for which no substitution has been made yet.
  private mutating func unify(
    _ lhs: AnyType, _ rhs: AnyType, querying relations: TypeRelations
  ) -> Bool {
    let (a, b) = (typeAssumptions[lhs], typeAssumptions[rhs])
    if relations.areEquivalent(a, b) { return true }

    switch (a.base, b.base) {
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
      goals[stale[i]].modifyTypes({ (type) in
        let u = typeAssumptions.reify(type, withVariables: .keep)
        changed = changed || (type != u)
        return u
      })

      if changed {
        log("- refresh \(goals[stale[i]])")
        fresh.append(stale.remove(at: i))
      }
    }
  }

  /// Returns the rank of `lhs` relative ro `rhs` comparing types using `checker` to query type
  /// relations and resolve names.
  private func rank(
    _ lhs: Solution,
    _ rhs: Solution,
    querying checker: inout TypeChecker
  ) -> StrictPartialOrdering {
    var ranking: StrictPartialOrdering = .equal
    var namesInCommon = 0

    for (n, lhsDeclRef) in lhs.bindingAssumptions {
      guard let rhsDeclRef = rhs.bindingAssumptions[n] else { continue }
      namesInCommon += 1

      // Nothing to do if both functions have the binding.
      if lhsDeclRef == rhsDeclRef { continue }
      let lhs = checker.declTypes[lhsDeclRef.decl!]!
      let rhs = checker.declTypes[rhsDeclRef.decl!]!

      switch compareSpecificity(lhs, rhs, at: checker.ast[n].site, querying: &checker) {
      case .ascending:
        if ranking == .descending { return nil }
        ranking = .ascending
      case .descending:
        if ranking == .ascending { return nil }
        ranking = .descending
      default:
        return nil
      }
    }

    if lhs.bindingAssumptions.count < rhs.bindingAssumptions.count {
      if namesInCommon == lhs.bindingAssumptions.count {
        return ranking != .ascending ? .descending : nil
      } else {
        return nil
      }
    }

    if lhs.bindingAssumptions.count > rhs.bindingAssumptions.count {
      if namesInCommon == rhs.bindingAssumptions.count {
        return ranking != .descending ? .ascending : nil
      } else {
        return nil
      }
    }

    return namesInCommon == lhs.bindingAssumptions.count ? ranking : nil
  }

  /// Compares `lhs` and `rhs` and returns whether one is more specific than the other.
  ///
  /// `t1` is more specific than `t2` if both are callable types with the same labels and `t1`
  /// accepts stricly less arguments than `t2`.
  private func compareSpecificity(
    _ lhs: AnyType, _ rhs: AnyType, at site: SourceRange, querying checker: inout TypeChecker
  ) -> StrictPartialOrdering {
    guard
      let l = lhs.base as? CallableType,
      let r = rhs.base as? CallableType
    else { return nil }

    guard
      l.inputs.count == r.inputs.count,
      l.inputs.elementsEqual(r.inputs, by: { $0.label == $1.label })
    else { return nil }

    let lRefinesR = isMoreSpecific(lhs, rhs, at: site, querying: &checker)
    let rRefinesL = isMoreSpecific(rhs, lhs, at: site, querying: &checker)

    if lRefinesR {
      return rRefinesL ? nil : .ascending
    }
    if rRefinesL {
      return lRefinesR ? nil : .descending
    }
    return nil
  }

  /// Returns `true` iff `lhs` is more specific than `rhs`.
  ///
  /// - Requires: `lhs` and `rhs` are `CallableType`s.
  private func isMoreSpecific(
    _ lhs: AnyType,
    _ rhs: AnyType,
    at site: SourceRange,
    querying checker: inout TypeChecker
  ) -> Bool {
    // Open the right operand.
    let openedRight = checker.open(type: rhs)
    var constraints = openedRight.constraints

    // Create pairwise subtyping constraints on the parameters.
    let l = lhs.skolemized.base as! CallableType
    let r = openedRight.shape.base as! CallableType
    for (a, b) in zip(l.inputs, r.inputs) {
      constraints.insert(SubtypingConstraint(a.type, b.type, origin: .init(.binding, at: site)))
    }

    // Solve the constraint system.
    var s = ConstraintSystem(constraints, in: scope, loggingTrace: isLoggingEnabled)
    return !s.solution(&checker).diagnostics.containsError
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
    for g in fresh {
      log("- - \"\(goals[g])\"")
      log("  - \"\(goals[g].origin)\"")
    }
    log("stale:")
    for g in stale {
      log("- - \"\(goals[g])\"")
      log("  - \"\(goals[g].origin)\"")
    }
  }

}

/// A set of constraint system solutions resulting from the explorations of the different choices
/// of a disjunctive constraint.
private struct Explorations<T: DisjunctiveConstraintProtocol> {

  /// The solution of an exploration for a particular choice.
  typealias Element = (choice: T.Predicate, solution: Solution)

  /// The results of the exploration.
  private(set) var elements: [Element] = []

  /// The score of the solutions in this set.
  private var score = Solution.Score.worst

  /// Creates an empty set.
  init() {}

  /// Inserts `newResult` into this set if its solution is better than or incomparable to any
  /// of the solutions currently in the set, ranking solutions with `rank`.
  mutating func insert(
    _ newResult: Element,
    rankingSolutionWith rank: (Solution, Solution) -> StrictPartialOrdering
  ) {
    // Rank solutions based on the name bindings they make. `s1` refines `s2` iff it has a better
    // score than `s2` or if it has the same score but makes at least one more specific binding
    // than `s2` and no binding less specific than `s2`.
    if newResult.solution.score > score { return }

    // Fast path: if the new solution has a better score, discard all others.
    if elements.isEmpty || (newResult.solution.score < score) {
      score = newResult.solution.score
      elements = [newResult]
      return
    }

    // Slow path: inspect how the new solution compares with the ones we have.
    var shouldInsert = false
    var i = 0
    while i < elements.count {
      switch rank(newResult.solution, elements[i].solution) {
      case .some(.equal):
        // The new solution is equal; discard it.
        return
      case .some(.descending):
        // The new solution is coarser; discard it unless it's better than another one.
        i += 1
      case .some(.ascending):
        // The new solution is finer; keep it and discard the old one.
        elements.remove(at: i)
        shouldInsert = true
      case nil:
        // The new solution is incomparable; keep it.
        i += 1
        shouldInsert = true
      }
    }

    if shouldInsert {
      elements.append(newResult)
    }
  }

}

extension Constraint {

  /// Returns whether `self` is heuristically simpler to solve than `other`.
  fileprivate func simpler(than other: Constraint) -> Bool {
    switch self {
    case is EqualityConstraint:
      return !(other is EqualityConstraint)

    case let l as any DisjunctiveConstraintProtocol:
      if let r = other as? any DisjunctiveConstraintProtocol {
        if l.choices.count == r.choices.count {
          let x = l.choices.reduce(0, { $0 + $1.constraints.count })
          let y = r.choices.reduce(0, { $0 + $1.constraints.count })
          return x < y
        } else {
          return (l.choices.count < r.choices.count)
        }
      } else {
        return false
      }

    default:
      return other is (any DisjunctiveConstraintProtocol)
    }
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
  origin: ConstraintOrigin
) -> Constraint {
  // If there aren't any type variable in neither `subtype` nor `supertype`, there's nothing to
  // infer and we can return a regular subtyping constraints.
  if !subtype[.hasVariable] && !supertype[.hasVariable] {
    return SubtypingConstraint(subtype, supertype, origin: origin)
  }

  // In other cases, we'll need two explorations. The first will unify `subtype` and `supertype`
  // and the other will try to infer them from the other constraints in the system.
  let alternative: Constraint
  if supertype.isLeaf {
    // If the supertype is a leaf, the subtype can only the same type or `Never`.
    alternative = EqualityConstraint(subtype, .never, origin: origin)
  } else {
    // Otherwise, the subtype can be any type upper-bounded by the supertype.
    alternative = SubtypingConstraint(subtype, supertype, strictly: true, origin: origin)
  }

  return DisjunctionConstraint(
    choices: [
      .init(constraints: [EqualityConstraint(subtype, supertype, origin: origin)], penalties: 0),
      .init(constraints: [alternative], penalties: 1),
    ],
    origin: origin)
}
