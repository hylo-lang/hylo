/// A solution returned by a constraint solver.
struct Solution {

  init(
    typeAssumptions: [TypeVariable: Type],
    bindingAssumptions: [NodeID<NameExpr>: DeclRef],
    penalties: Int,
    diagnostics: [Diagnostic]) {
    self.typeAssumptions = typeAssumptions
    self.bindingAssumptions = bindingAssumptions
    self.penalties = penalties
    self.diagnostics = diagnostics
  }

  /// A policy for substituting type variales during reification.
  enum SubstitutionPolicy {

    /// Substitute free variables by error types.
    case substituteByError

    /// Do not substitute free variables.
    case keep

  }

  /// The score of a solution.
  struct Score: Comparable {

    /// The number of errors encountered by the solver.
    var errorCount: Int

    /// The penalties of the choices made by the solver.
    var penalties: Int

    /// The worst representable solution score.
    static let worst = Score(errorCount: Int.max, penalties: Int.max)

    static func < (l: Self, r: Self) -> Bool {
      l.errorCount == r.errorCount
        ? l.penalties < r.penalties
        : l.errorCount < r.errorCount
    }

  }

  /// The type assumptions made by the solver.
  private let typeAssumptions: [TypeVariable: Type]

  /// The name binding assumptions made by the solver.
  let bindingAssumptions: [NodeID<NameExpr>: DeclRef]

  /// The penalties of the solution.
  private var penalties: Int

  /// The diagnostics of the errors associated with the solution.
  private(set) var diagnostics: [Diagnostic]

  /// The score of the solution.
  var score: Score { Score(errorCount: diagnostics.count, penalties: penalties) }

  /// Reifies the given type, substituting each free variable by its corresponding binding.
  func reify(_ type: Type, withVariables substitutionPolicy: SubstitutionPolicy) -> Type {
    type.transform({ type in
      if case .variable(let v) = type {
        // Substitute variables.
        if let t = typeAssumptions[v] {
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

  /// Adds `d` to the list of diagnostics associated with this solution.
  internal mutating func diagnose(_ d: Diagnostic) {
    diagnostics.append(d)
  }
}
