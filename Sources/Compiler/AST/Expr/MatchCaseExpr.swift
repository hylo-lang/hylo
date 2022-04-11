/// A case in a match expression.
public struct MatchCaseExpr: Expr, LexicalScope {

  public static let kind = NodeKind.matchCaseExpr

  public enum Body: Hashable {

    /// An expression body.
    case expr(AnyExprIndex)

    /// A block body.
    case block(NodeIndex<BraceStmt>)

  }

  /// The pattern of the case.
  public var pattern: AnyPatternIndex

  /// The condition of the case, if any.
  public var condition: AnyExprIndex?

  /// The body of the case.
  public var body: SourceRepresentable<Body>

}
