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
  let scope: AnyNodeID

  /// The fresh constraints to solve.
  var fresh: [LocatableConstraint] = []

  /// The constraints that are currently stale.
  var stale: [LocatableConstraint] = []

  /// The assumptions of the type solver.
  var assumptions = SubstitutionMap()

  /// The current penalities of the solver's solution.
  var penalities: Int = 0

  /// The current set of errors the solver encountered.
  var errors: [TypeError] = []

  /// The best solution computed so far.
  var best: Solution?

  /// The current score of the solver's solution.
  var score: Solution.Score {
    Solution.Score(errorCount: errors.count, penalities: penalities)
  }

  /// Solves the constraints.
  mutating func solve() -> Solution {
    while let constraint = fresh.popLast() {
      // Make sure the current solution is still worth exploring.
      if let best = best, score > best.score { return best }

      let result: SolverResult
      switch constraint.constraint {
      case .conformance(let l, let traits):
        result = solve(l, conformsTo: traits)
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

    return solution()
  }

  private mutating func solve(_ l: Type, conformsTo traits: Set<TraitType>) -> SolverResult {
    let l = assumptions[l]

    switch l {
    case .variable:
      // Postpone the constraint if `L` is still unknown.
      return .postpone

    case .product:
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

  /// Creates a solution from the current state.
  private func solution() -> Solution {
    assert(fresh.isEmpty)
    var s = Solution(
      assumptions: assumptions.flattened(),
      penalities: penalities,
      errors: errors)
    for constraint in stale {
      s.errors.append(TypeError(kind: .staleConstaint, cause: constraint))
    }
    return s
  }

}
