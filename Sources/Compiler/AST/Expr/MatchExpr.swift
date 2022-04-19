/// A match expression.
public struct MatchExpr: Expr {

  public static let kind = NodeKind.matchExpr

  /// The subject of the match.
  public var subject: AnyExprID

  /// The cases of the match.
  public var cases: [NodeID<MatchCaseExpr>]

  public init(subject: AnyExprID, cases: [NodeID<MatchCaseExpr>]) {
    self.subject = subject
    self.cases = cases
  }

}
