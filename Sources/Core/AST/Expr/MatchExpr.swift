/// A match expression.
public struct MatchExpr: Expr {

  public let origin: SourceRange

  /// The subject of the match.
  public let subject: AnyExprID

  /// The cases of the match.
  public let cases: [NodeID<MatchCase>]

  public init(subject: AnyExprID, cases: [NodeID<MatchCase>], origin: SourceRange) {
    self.origin = origin
    self.subject = subject
    self.cases = cases
  }

}
