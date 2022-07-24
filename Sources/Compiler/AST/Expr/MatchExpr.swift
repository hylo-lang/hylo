/// A match expression.
public struct MatchExpr: Expr {

  public static let kind = NodeKind.matchExpr

  /// The subject of the match.
  public var subject: AnyExprID

  /// The cases of the match.
  public var cases: [NodeID<MatchCase>]

  public init(subject: AnyExprID, cases: [NodeID<MatchCase>]) {
    self.subject = subject
    self.cases = cases
  }

}
