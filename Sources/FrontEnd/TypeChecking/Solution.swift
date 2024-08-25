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
  private(set) var typeAssumptions: SubstitutionMap

  /// The name binding assumptions made by the solver.
  private(set) var bindingAssumptions: BindingMap

  /// A map from call expression to its operands after desugaring and implicit resolution.
  private(set) var callOperands: [CallID: [ArgumentResolutionResult]]

  /// The penalties of the solution.
  private(set) var penalties: Int

  /// The diagnostics of the errors associated with the solution.
  private(set) var diagnostics: DiagnosticSet

  /// The constraints that could not be solved.
  private(set) var stale: [Constraint]

  /// Creates an empty solution.
  init() {
    self.init(
      substitutions: .init(), bindings: [:], callOperands: [:],
      penalties: 0, diagnostics: [], stale: [])
  }

  /// Creates an instance with the given properties.
  init(
    substitutions typeAssumptions: SubstitutionMap,
    bindings bindingAssumptions: [NameExpr.ID: DeclReference],
    callOperands: [CallID: [ArgumentResolutionResult]],
    penalties: Int,
    diagnostics: DiagnosticSet,
    stale: [Constraint]
  ) {
    self.typeAssumptions = typeAssumptions
    self.bindingAssumptions = bindingAssumptions
    self.callOperands = callOperands
    self.penalties = penalties
    self.diagnostics = diagnostics
    self.stale = stale
  }

  /// `true` iff the solution has no error.
  var isSound: Bool {
    !diagnostics.containsError && stale.isEmpty
  }

  /// The score of the solution.
  var score: Score {
    Score(errorCount: diagnostics.elements.count + stale.count, penalties: penalties)
  }

  /// Incorporates `d` into `self`.
  mutating func incorporate(_ d: Diagnostic) {
    diagnostics.insert(d)
  }

  /// Removes the type and binding assumptions that aren't in `other` and incorporate the
  /// penalties and diagnostics of `other` into `self`.
  mutating func formIntersection(_ other: Self) {
    typeAssumptions.formIntersection(other.typeAssumptions)
    bindingAssumptions.formIntersection(other.bindingAssumptions)
    diagnostics.formIntersection(other.diagnostics)
    penalties = max(penalties, other.penalties)
  }

}

extension Solution: CustomStringConvertible {

  public var description: String {
    var result = ""

    result.append("typeAssumptions:\n")
    for (k, v) in typeAssumptions.types {
      result.append("  \(k) : \(v)\n")
    }

    result.append("bindingAssumptions:\n")
    for (k, v) in bindingAssumptions {
      result.append("  \(k) : \(v)\n")
    }

    result.append("penalties: \(penalties)\n")

    result.append("diagnostics: \(diagnostics.elements.count)\n")

    result.append("stale:\n")
    for c in stale {
      result.append("  \(c)")
    }

    return result
  }

}
