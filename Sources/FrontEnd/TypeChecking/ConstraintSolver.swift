import Core
import Utils

/// A constraint system solver.
struct ConstraintSolver {

  /// The solution of an exploration given a particular choice.
  private typealias ExploratinResult<T> = (choice: T, solution: Solution)

  /// The scope in which the constraints are solved.
  private let scope: AnyScopeID

  /// The fresh constraints to solve.
  private var fresh: [Constraint] = []

  /// The constraints that are currently stale.ß
  private var stale: [Constraint] = []

  /// The type assumptions of the solver.
  private var typeAssumptions = SubstitutionMap()

  /// The binding assumptions of the solver.
  private var bindingAssumptions: [NodeID<NameExpr>: DeclRef] = [:]

  /// The current penalties of the solver's solution.
  private var penalties: Int = 0

  /// The diagnostics of the errors the solver encountered.
  private var diagnostics: DiagnosticSet = []

  /// The score of the best solution computed so far.
  private var best = Solution.Score.worst

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

  /// The current score of the solver's solution.
  private var score: Solution.Score {
    Solution.Score(errorCount: diagnostics.elements.count, penalties: penalties)
  }

  /// Returns the best solution solving the constraints in `self` using `checker` to query type
  /// relations and resolve names.
  mutating func solution(_ checker: inout TypeChecker) -> Solution {
    solveConstraints(&checker)!
  }

  /// Returns the best solution solving the constraints in `self` using `checker` to query type
  /// relations and resolve names, or `nil` if no solution with a score better than `self.best`
  /// can be found.
  private mutating func solveConstraints(_ checker: inout TypeChecker) -> Solution? {
    logState()
    log("steps:")

    while let constraint = fresh.popLast() {
      // Make sure the current solution is still worth exploring.
      if score > best {
        log("- abort")
        return nil
      }

      switch constraint {
      case let c as ConformanceConstraint:
        solve(conformance: c, using: &checker)
      case let c as LiteralConstraint:
        solve(literal: c, using: &checker)
      case let c as EqualityConstraint:
        solve(equality: c, using: &checker)
      case let c as SubtypingConstraint:
        solve(subtyping: c, using: &checker)
      case let c as ParameterConstraint:
        solve(parameter: c, using: &checker)
      case let c as MemberConstraint:
        solve(member: c, using: &checker)
      case let c as FunctionCallConstraint:
        solve(functionCall: c, using: &checker)
      case let c as DisjunctionConstraint:
        return solve(disjunction: c, using: &checker)
      case let c as OverloadConstraint:
        return solve(overload: c, using: &checker)
      default:
        unreachable()
      }

      if fresh.isEmpty { refreshLiteralConstraints() }
    }

    return finalize()
  }

  /// Eliminates `L : T1 & ... & Tn` if the solver has enough information to check whether or not
  /// `L` conforms to each trait `Ti`. Otherwise, postpones the constraint.
  private mutating func solve(
    conformance constraint: ConformanceConstraint,
    using checker: inout TypeChecker
  ) {
    log("- solve: \"\(constraint)\"")
    indentation += 1
    defer { indentation -= 1 }
    log("actions:")

    let goal = constraint.modifyingTypes({ typeAssumptions[$0] })
    var missingTraits: Set<TraitType>

    switch goal.subject.base {
    case is TypeVariable:
      // Postpone the solving if `L` is still unknown.
      postpone(goal)
      return

    case is BuiltinType:
      // Built-in types are `Sinkable`.
      missingTraits = constraint.traits.subtracting(
        [checker.ast.coreTrait(named: "Sinkable")!])

    default:
      missingTraits = goal.traits.subtracting(
        checker.conformedTraits(of: goal.subject, in: scope) ?? [])
    }

    if !missingTraits.isEmpty {
      report(
        missingTraits.map({ .error(goal.subject, doesNotConformTo: $0, at: goal.cause.site) }))
    }
  }

  /// Eliminates `(L ?? D) : T` if the solver has enough information to check whether `L` conforms
  /// to `T`. Otherwise, postpones the constraint.
  private mutating func solve(
    literal constraint: LiteralConstraint,
    using checker: inout TypeChecker
  ) {
    log("- solve: \"\(constraint)\"")
    indentation += 1
    defer { indentation -= 1 }
    log("actions:")

    let goal = constraint.modifyingTypes({ typeAssumptions[$0] })

    // The constraint is trivially solved if `L` is equal to `D`.
    if checker.relations.areEquivalent(goal.subject, goal.defaultSubject) { return }

    // Check that `L` conforms to `T` or postpone if it's still unknown.
    if goal.subject.base is TypeVariable {
      postpone(goal)
    } else {
      let conformedTraits = checker.conformedTraits(of: goal.subject, in: scope) ?? []

      if conformedTraits.contains(goal.literalTrait) {
        // Add a penalty if `L` isn't `D`.
        penalties += 1
      } else {
        report(.error(goal.subject, doesNotConformTo: goal.literalTrait, at: goal.cause.site))
      }
    }
  }

  /// Eliminates `L == R` by unifying `L` with `R`.
  private mutating func solve(
    equality constraint: EqualityConstraint,
    using checker: inout TypeChecker
  ) {
    log("- solve: \"\(constraint)\"")
    indentation += 1
    defer { indentation -= 1 }
    log("actions:")

    let goal = constraint.modifyingTypes({ typeAssumptions[$0] })

    // Handle trivially satisified constraints.
    if checker.relations.areEquivalent(goal.left, goal.right) { return }

    switch (goal.left.base, goal.right.base) {
    case (let tau as TypeVariable, _):
      assume(tau, equals: goal.right)

    case (_, let tau as TypeVariable):
      assume(tau, equals: goal.left)

    case (let l as TupleType, let r as TupleType):
      // Make sure `L` and `R` are structurally compatible.
      if !l.labels.elementsEqual(r.labels) {
        report(.error(type: goal.left, incompatibleWith: goal.right, at: goal.cause.site))
        return
      }

      // Break down the constraint.
      for i in 0 ..< l.elements.count {
        solve(
          equality: .init(l.elements[i].type, r.elements[i].type, because: goal.cause),
          using: &checker)
      }

    case (let l as LambdaType, let r as LambdaType):
      // Parameter labels must match.
      if !l.labels.elementsEqual(r.labels) {
        report(.error(type: ^l, incompatibleWith: ^r, at: goal.cause.site))
        return
      }

      // Break down the constraint.
      for i in 0 ..< l.inputs.count {
        solve(
          equality: .init(l.inputs[i].type, r.inputs[i].type, because: goal.cause),
          using: &checker)
      }

      solve(equality: .init(l.output, r.output, because: goal.cause), using: &checker)
      solve(equality: .init(l.environment, r.environment, because: goal.cause), using: &checker)

    case (let l as MethodType, let r as MethodType):
      // Parameter labels must match.
      if !l.labels.elementsEqual(r.labels) {
        report(.error(type: ^l, incompatibleWith: ^r, at: goal.cause.site))
        return
      }

      // Capabilities must match.
      if l.capabilities != r.capabilities {
        report(.error(type: ^l, incompatibleWith: ^r, at: goal.cause.site))
        return
      }

      // Break down the constraint.
      for (l, r) in zip(l.inputs, r.inputs) {
        solve(equality: .init(l.type, r.type, because: goal.cause), using: &checker)
      }
      solve(equality: .init(l.output, r.output, because: goal.cause), using: &checker)
      solve(equality: .init(l.receiver, r.receiver, because: goal.cause), using: &checker)

    case (let l as ParameterType, let r as ParameterType):
      if l.access != r.access {
        report(.error(type: ^l, incompatibleWith: ^r, at: goal.cause.site))
        return
      }
      solve(equality: .init(l.bareType, r.bareType, because: goal.cause), using: &checker)

    case (let l as RemoteType, let r as RemoteType):
      if l.access != r.access {
        report(.error(type: ^l, incompatibleWith: ^r, at: goal.cause.site))
        return
      }
      solve(equality: .init(l.bareType, r.bareType, because: goal.cause), using: &checker)

    default:
      report(.error(type: goal.left, incompatibleWith: goal.right, at: goal.cause.site))
    }
  }

  /// Eliminates `L <: R` or `L < R` if the solver has enough information to check that `L` is
  /// subtype of `R` or must be unified with `R`. Otherwise, postpones the constraint.
  private mutating func solve(
    subtyping constraint: SubtypingConstraint,
    using checker: inout TypeChecker
  ) {
    log("- solve: \"\(constraint)\"")
    indentation += 1
    defer { indentation -= 1 }
    log("actions:")

    let goal = constraint.modifyingTypes({ typeAssumptions[$0] })

    // Handle cases where `L` is equal to `R`.
    if checker.relations.areEquivalent(goal.left, goal.right) {
      if goal.isStrict { diagnoseFailureToSove(goal) }
      return
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

    case (_ as TypeVariable, _):
      // The type variable is below a more concrete type. We should compute the "meet" of all types
      // coercible to `R` and that are above `L`, but that set is unbounded unless `R` is a leaf.
      // If it isn't, we have no choice but to postpone the constraint.
      if goal.right.isLeaf {
        solve(equality: .init(constraint), using: &checker)
      } else if goal.isStrict {
        postpone(goal)
      } else {
        schedule(inferenceConstraint(goal.left, isSubtypeOf: goal.right, because: goal.cause))
      }

    case (_, _ as ExistentialType):
      // All types conform to any.
      if goal.right == .any { return }
      fatalError("not implemented")

    case (let l as LambdaType, let r as LambdaType):
      // Environments must be equal.
      solve(
        equality: EqualityConstraint(l.environment, r.environment, because: goal.cause),
        using: &checker)

      // Parameter labels must match.
      if !l.labels.elementsEqual(r.labels) {
        diagnoseFailureToSove(goal)
        return
      }

      // Parameters are contravariant; return types are covariant.
      for (a, b) in zip(l.inputs, r.inputs) {
        schedule(SubtypingConstraint(b.type, a.type, because: goal.cause))
      }
      schedule(SubtypingConstraint(l.output, r.output, because: goal.cause))

    case (let l as SumType, _ as SumType):
      // If both types are sums, all elements in `L` must be contained in `R`.
      for e in l.elements {
        solve(subtyping: .init(e, goal.right, because: goal.cause), using: &checker)
      }

    case (_, let r as SumType):
      // If `R` is a sum type and `L` isn't, then `L` must be contained in `R`.
      for e in r.elements {
        if checker.relations.areEquivalent(goal.left, e) { return }
      }

      // Postpone the constraint if either `L` or `R` contains variables. Otherwise, `L` is not
      // subtype of `R`.
      if goal.left[.hasVariable] || goal.right[.hasVariable] {
        postpone(goal)
      } else {
        diagnoseFailureToSove(goal)
      }

    default:
      if goal.isStrict {
        diagnoseFailureToSove(goal)
      } else {
        solve(equality: EqualityConstraint(goal), using: &checker)
      }
    }
  }

  /// Diagnoses a failure to solve `goal`.
  private mutating func diagnoseFailureToSove(_ goal: SubtypingConstraint) {
    switch goal.cause.kind {
    case .initializationWithHint:
      report(.error(cannotInitialize: goal.left, with: goal.right, at: goal.cause.site))

    case .initializationWithPattern:
      report(.error(goal.left, doesNotMatchPatternAt: goal.cause.site))

    default:
      if goal.isStrict {
        report(.error(goal.left, isNotStrictSubtypeOf: goal.right, at: goal.cause.site))
      } else {
        report(.error(goal.left, isNotSubtypeOf: goal.right, at: goal.cause.site))
      }
    }
  }

  /// Eliminates `L ⤷ R` if the solver has enough information to choose whether the constraint can
  /// be simplified as equality or subtyping. Otherwise, postpones the constraint.
  private mutating func solve(
    parameter constraint: ParameterConstraint,
    using checker: inout TypeChecker
  ) {
    log("- solve: \"\(constraint)\"")
    indentation += 1
    defer { indentation -= 1 }
    log("actions:")

    let goal = constraint.modifyingTypes({ typeAssumptions[$0] })

    // Handle trivially satisified constraints.
    if checker.relations.areEquivalent(goal.left, goal.right) { return }

    switch goal.right.base {
    case is TypeVariable:
      // Postpone the solving until we can infer the parameter passing convention of `R`.
      postpone(goal)

    case let p as ParameterType:
      // Either `L` is equal to the bare type of `R`, or it's a. Note: the equality requirement for
      // arguments passed mutably is verified after type inference.
      schedule(SubtypingConstraint(goal.left, p.bareType, because: goal.cause))

    default:
      report(.error(invalidParameterType: goal.right, at: goal.cause.site))
    }
  }

  /// Simplifies `L.m == R` as an overload or equality constraint unifying `R` with the type of
  /// `L.m` if the solver has enough information to resolve `m` as a member. Otherwise, postones
  /// the constraint.
  private mutating func solve(
    member constraint: MemberConstraint,
    using checker: inout TypeChecker
  ) {
    log("- solve: \"\(constraint)\"")
    indentation += 1
    defer { indentation -= 1 }
    log("actions:")

    let goal = constraint.modifyingTypes({ typeAssumptions[$0] })

    // Postpone the solving if `L` is still unknown.
    if goal.subject.base is TypeVariable {
      postpone(goal)
      return
    }

    let matches = checker.lookup(goal.memberName.stem, memberOf: goal.subject, in: scope)
      .compactMap({ checker.decl(in: $0, named: goal.memberName) })

    // Generate the list of candidates.
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
      report(.error(undefinedName: "\(goal.memberName)", at: goal.cause.site))
      return
    }

    // If there's only one candidate, solve an equality constraint direcly.
    if let pick = candidates.uniqueElement {
      solve(equality: .init(pick.type, goal.memberType, because: goal.cause), using: &checker)

      log("- assume: \"\(goal.memberExpr) &> \(pick.reference)\"")
      bindingAssumptions[goal.memberExpr] = pick.reference
      return
    }

    // If there are several candidates, create a overload constraint.
    schedule(
      OverloadConstraint(
        goal.memberExpr, withType: goal.memberType, refersToOneOf: candidates,
        because: goal.cause))
  }

  /// Simplifies `F(P1, ..., Pn) -> R` as equality constraints unifying the parameters and return
  /// type of `F` with `P1, ..., Pn` and `R`, respectively.
  private mutating func solve(
    functionCall constraint: FunctionCallConstraint,
    using checker: inout TypeChecker
  ) {
    log("- solve: \"\(constraint)\"")
    indentation += 1
    defer { indentation -= 1 }
    log("actions:")

    let goal = constraint.modifyingTypes({ typeAssumptions[$0] })

    // Postpone the solving if `F` is still unknown.
    if goal.calleeType.base is TypeVariable {
      postpone(goal)
      return
    }

    // Make sure `F` is callable.
    guard let callee = goal.calleeType.base as? CallableType else {
      report(.error(nonCallableType: goal.calleeType, at: goal.cause.site))
      return
    }

    // Make sure `F` structurally matches the given parameter list.
    if goal.labels.count != callee.labels.count {
      report(.error(incompatibleParameterCountAt: goal.cause.site))
      return
    } else if !goal.labels.elementsEqual(callee.labels) {
      report(.error(labels: goal.labels, incompatibleWith: callee.labels, at: goal.cause.site))
      return
    }

    // Break down the constraint.
    for (l, r) in zip(callee.inputs, goal.parameters) {
      solve(equality: .init(l.type, r.type, because: goal.cause), using: &checker)
    }
    solve(equality: .init(callee.output, goal.returnType, because: goal.cause), using: &checker)
  }

  /// Attempts to solve the remaining constraints for each individual choice in `disjunction` and
  /// returns the best solution.
  private mutating func solve(
    disjunction constraint: DisjunctionConstraint,
    using checker: inout TypeChecker
  ) -> Solution? {
    log("- solve: \"\(constraint)\"")
    indentation += 1
    defer { indentation -= 1 }
    log("actions:")

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
      cause: .error(ambiguousDisjunctionAt: constraint.cause.site))
  }

  /// Attempts to solve the remaining constraints with each individual choice in `overload` and
  /// returns the best solution.
  private mutating func solve(
    overload constraint: OverloadConstraint,
    using checker: inout TypeChecker
  ) -> Solution? {
    log("- solve: \"\(constraint)\"")
    indentation += 1
    defer { indentation -= 1 }
    log("actions:")

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
      cause: .error(
        ambiguousUse: constraint.overloadedExpr,
        in: checker.ast,
        candidates: results.compactMap(\.choice.reference.decl)))
  }

  /// Solves the remaining constraint with each given choice and returns the best solutions.
  private mutating func explore<Choices: Collection>(
    _ choices: Choices,
    using checker: inout TypeChecker,
    configuringSubSolversWith configureSubSolver: (inout Self, Choices.Element) -> Void
  ) -> [ExploratinResult<Choices.Element>] where Choices.Element: Choice {
    log("- fork:")
    indentation += 1
    defer { indentation -= 1 }

    /// The results of the exploration.
    var results: [ExploratinResult<Choices.Element>] = []

    for choice in choices {
      // Don't bother if there's no chance to find a better solution.
      var underestimatedChoiceScore = score
      underestimatedChoiceScore.penalties += choice.penalties
      if underestimatedChoiceScore > best {
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
    _ newResult: ExploratinResult<T>,
    into bestResults: inout [ExploratinResult<T>],
    using checker: inout TypeChecker
  ) {
    // Rank solutions based on the name bindings they make. `s1` refines `s2` iff it has a better
    // score than `s2` or if it has the same score but makes at least one more specific binding
    // than `s2` and no binding less specific than `s2`.
    if newResult.solution.score > best { return }

    // Fast path: if the new solution has a better score, discard all others.
    if bestResults.isEmpty || (newResult.solution.score < best) {
      best = newResult.solution.score
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

  /// Schedules `constraint` to be solved in the future.
  private mutating func schedule(_ constraint: Constraint) {
    log("- schedule: \"\(constraint)\"")
    insert(fresh: constraint)
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
  private mutating func finalize() -> Solution {
    assert(fresh.isEmpty)

    diagnostics.formUnion(stale.map(Diagnostic.error(staleConstraint:)))
    return Solution(
      typeAssumptions: typeAssumptions.optimized(),
      bindingAssumptions: bindingAssumptions,
      penalties: penalties,
      diagnostics: diagnostics)
  }

  /// Creates an ambiguous solution.
  private func formAmbiguousSolution<T>(
    _ results: [ExploratinResult<T>],
    cause: Diagnostic
  ) -> Solution {
    var types = results[0].solution.typeAssumptions
    var bindings = results[0].solution.bindingAssumptions
    var penalties = results[0].solution.score.penalties
    var diagnostics = results[0].solution.diagnostics
    diagnostics.insert(cause)

    for result in results.dropFirst() {
      types.formIntersection(result.solution.typeAssumptions)
      bindings.formIntersection(result.solution.bindingAssumptions)
      penalties = max(penalties, result.solution.score.penalties)
      diagnostics.formUnion(result.solution.diagnostics)
    }

    return Solution(
      typeAssumptions: types,
      bindingAssumptions: bindings,
      penalties: penalties,
      diagnostics: diagnostics)
  }

  /// Adds `d` to `self.diagnostics`.
  private mutating func report(_ d: Diagnostic) {
    log("- fail")
    diagnostics.insert(d)
  }

  /// Adds `batch` to `self.diagnostics`.
  private mutating func report<S: Sequence<Diagnostic>>(_ batch: S) {
    log("- fail")
    diagnostics.formUnion(batch)
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
