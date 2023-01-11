import Core

/// A solution returned by a constraint solver.
struct Solution {

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
  let typeAssumptions: SubstitutionMap

  /// The name binding assumptions made by the solver.
  let bindingAssumptions: BindingMap

  /// The penalties of the solution.
  private var penalties: Int

  /// The diagnostics of the errors associated with the solution.
  private(set) var diagnostics: [Diagnostic]

  /// Creates an empty solution.
  init() {
    self.init(typeAssumptions: [:], bindingAssumptions: [:], penalties: 0, diagnostics: [])
  }

  /// Creates an instance with the given properties.
  init(
    typeAssumptions: SubstitutionMap,
    bindingAssumptions: [NodeID<NameExpr>: DeclRef],
    penalties: Int,
    diagnostics: [Diagnostic]
  ) {
    self.typeAssumptions = typeAssumptions
    self.bindingAssumptions = bindingAssumptions
    self.penalties = penalties
    self.diagnostics = diagnostics
  }

  /// The score of the solution.
  var score: Score { Score(errorCount: diagnostics.count, penalties: penalties) }

}
