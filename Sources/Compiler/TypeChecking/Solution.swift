/// A solution returned by a constraint solver.
struct Solution {

  /// The score of a solution.
  struct Score: Comparable {

    let errorCount: Int

    let penalties: Int

    static let worst = Score(errorCount: Int.max, penalties: Int.max)

    static func < (l: Self, r: Self) -> Bool {
      l.errorCount == r.errorCount
        ? l.penalties < r.penalties
        : l.errorCount < r.errorCount
    }

  }

  /// The assumptions made by the constraint solver.
  var assumptions: [TypeVariable: Type]

  /// The penalties of the solution.
  var penalties: Int

  /// The errors associated with the solution.
  var errors: [TypeError]

  /// The score of the solution.
  var score: Score { Score(errorCount: errors.count, penalties: penalties) }

  /// Reifies the given type, substituting each free variable by its corresponding binding.
  func reify(_ type: Type) -> Type {
    type.transform({ type in
      if case .variable(let v) = type {
        return .stepInto(assumptions[v] ?? type)
      } else {
        return .stepInto(type)
      }
    })
  }
}
