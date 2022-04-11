/// A match expression.
public struct MatchExpr: Expr {

  public static let kind = NodeKind.matchExpr

  /// The subject of the match.
  public var subject: AnyExprIndex

  /// The cases of the match.
  public var cases: [NodeIndex<MatchCaseExpr>]

}
