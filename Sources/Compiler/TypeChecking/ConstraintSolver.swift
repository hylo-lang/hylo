import Utils

/// A constraint system solver.
struct ConstraintSolver {

  private enum SolverResult {

    case success

    case postpone

    case failure(TypeError.Kind)

  }

  /// A borrowed projection of the type checker that uses this constraint generator.
  var checker: TypeChecker!

  /// The scope in which the constraints are solved.
  let scope: AnyScopeID

  /// The fresh constraints to solve.
  var fresh: [LocatableConstraint] = []

  /// The constraints that are currently stale.
  var stale: [LocatableConstraint] = []

  /// The assumptions of the type solver.
  var assumptions = SubstitutionMap()

  /// The current penalties of the solver's solution.
  var penalties: Int = 0

  /// The current set of errors the solver encountered.
  var errors: [TypeError] = []

  /// The score of the best solution computed so far.
  var best = Solution.Score.worst

  /// The current score of the solver's solution.
  var score: Solution.Score {
    Solution.Score(errorCount: errors.count, penalties: penalties)
  }

  /// Solves the constraints and returns the best solution, or `nil` if a better solution has
  /// already been computed.
  mutating func solve() -> Solution? {
    while let constraint = fresh.popLast() {
      // Make sure the current solution is still worth exploring.
      if score > best { return nil }

      let result: SolverResult
      switch constraint.constraint {
      case .conformance(let l, let traits):
        result = solve(l, conformsTo: traits)
      case .equality(let l, let r):
        result = solve(l, equalsTo: r)
      case .disjunction:
        return solve(disjunction: constraint)
      default:
        fatalError("not implemented")
      }

      switch result {
      case .success:
        continue
      case .postpone:
        stale.append(constraint)
      case .failure(let kind):
        errors.append(TypeError(kind: kind, cause: constraint))
      }
    }

    return finalize()
  }

  private mutating func solve(_ l: Type, conformsTo traits: Set<TraitType>) -> SolverResult {
    let l = assumptions[l]

    switch l {
    case .variable:
      // Postpone the constraint if `L` is still unknown.
      return .postpone

    case .product, .tuple:
      let conformedTraits = checker.conformedTraits(of: l, inScope: scope) ?? []
      let nonConforming = traits.subtracting(conformedTraits)
      if nonConforming.isEmpty {
        return .success
      } else {
        return .failure(.doesNotConform(l, traits: traits, scope: scope))
      }

    default:
      fatalError("not implemented")
    }
  }

  private mutating func solve(_ l: Type, equalsTo r: Type) -> SolverResult {
    let l = assumptions[l]
    let r = assumptions[r]

    if l == r { return .success }

    switch (l, r) {
    case (.variable(let tau), _):
      assumptions.assign(r, to: tau)
      return .success

    case (_, .variable(let tau)):
      assumptions.assign(l, to: tau)
      return .success

    default:
      fatalError("not implemented")
    }
  }

  private mutating func solve(disjunction: LocatableConstraint) -> Solution {
    guard case .disjunction(let minterms) = disjunction.constraint else { unreachable() }

    var solutions: [Solution] = []
    for minterm in minterms {
      // Explore the result of this choice.
      var subsolver = self
      subsolver.penalties += minterm.penalties
      for constraint in minterm.constraints {
        subsolver.fresh.append(LocatableConstraint(
          constraint, node: disjunction.node, cause: disjunction.cause))
      }

      guard let solution = subsolver.solve() else { continue }
      if solution.score < best {
        best = solution.score
        solutions = [solution]
      } else if solution.score == best {
        // TODO: Avoid duplicates
        solutions.append(solution)
      }
    }

    assert(!solutions.isEmpty)
    if solutions.count == 1 {
      return solutions[0]
    } else {
      // TODO: Merge remaining solutions
      var s = solutions[0]
      s.errors.append(TypeError(kind: .ambiguousDisjunction, cause: disjunction))
      return s
    }
  }

  /// Creates a solution from the current state.
  private func finalize() -> Solution {
    assert(fresh.isEmpty)
    var s = Solution(
      assumptions: assumptions.flattened(),
      penalties: penalties,
      errors: errors)
    for constraint in stale {
      s.errors.append(TypeError(kind: .staleConstaint, cause: constraint))
    }
    return s
  }

}
