/// A solution returned by a constraint solver.
struct Solution {

  /// The score of a solution.
  struct Score: Comparable {

    let errorCount: Int

    let penalities: Int

    static let worst = Score(errorCount: Int.max, penalities: Int.max)

    static func < (l: Self, r: Self) -> Bool {
      l.errorCount == r.errorCount
        ? l.penalities < r.penalities
        : l.errorCount < r.errorCount
    }

  }

  /// The assumptions made by the constraint solver.
  var assumptions: [TypeVariable: Type]

  /// The penalities of the solution.
  var penalities: Int

  /// The errors associated with the solution.
  var errors: [TypeError]

  /// The score of the solution.
  var score: Score { Score(errorCount: errors.count, penalities: penalities) }

}
