/// A case in a match expression.
public struct MatchCaseExpr: Expr, LexicalScope {

  public static let kind = NodeKind.matchCaseExpr

  public enum Body: Hashable {

    /// An expression body.
    case expr(AnyExprID)

    /// A block body.
    case block(NodeID<BraceStmt>)

  }

  /// The pattern of the case.
  public var pattern: AnyPatternID

  /// The condition of the case, if any.
  public var condition: AnyExprID?

  /// The body of the case.
  public var body: SourceRepresentable<Body>

}
