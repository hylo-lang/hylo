/// A match expression.
public struct MatchExpr: Expr {

  public static let kind = NodeKind.matchExpr

  /// The subject of the match.
  public let subject: AnyExprID

  /// The cases of the match.
  public let cases: [NodeID<MatchCase>]

  public init(subject: AnyExprID, cases: [NodeID<MatchCase>]) {
    self.subject = subject
    self.cases = cases
  }

}
