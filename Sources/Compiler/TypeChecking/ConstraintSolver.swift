/// A constraint system solver.
struct ConstraintSolver {

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

  /// The score of the best solution computed so far.
  var bestScore: Solution.Score = .worst

  /// The current score of the solver's solution.
  var currentScore: Solution.Score {
    Solution.Score(errorCount: errors.count, penalities: penalities)
  }

  /// Solves the constraints.
  mutating func solve() -> Solution {
    solution()
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
