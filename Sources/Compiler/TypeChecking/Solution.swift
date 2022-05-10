/// A solution returned by a constraint solver.
struct Solution {

  /// A policy for substituting type variales during reification.
  enum SubstitutionPolicy {

    /// Substitute free variables by error types.
    case substituteByError

    /// Do not substitute free variables.
    case keep

  }

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

  /// The diagnostics of the errors associated with the solution.
  var diagnostics: [Diagnostic]

  /// The score of the solution.
  var score: Score { Score(errorCount: diagnostics.count, penalties: penalties) }

  /// Reifies the given type, substituting each free variable by its corresponding binding.
  func reify(_ type: Type, withVariables substitutionPolicy: SubstitutionPolicy) -> Type {
    type.transform({ type in
      if case .variable(let v) = type {
        // Substitute variables.
        if let t = assumptions[v] {
          return .stepInto(t)
        } else {
          switch substitutionPolicy {
          case .substituteByError:
            return .stepInto(.error(ErrorType()))
          case .keep:
            return .stepOver(type)
          }
        }
      } else {
        // Recursively visit other types.
        return .stepInto(type)
      }
    })
  }
}
