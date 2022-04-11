/// A conditional expression.
public struct CondExpr: Expr, LexicalScope {

  public static let kind = NodeKind.condExpr

  public enum Body: Hashable {

    /// An expression body.
    case expr(AnyExprIndex)

    /// A block body.
    case block(NodeIndex<BraceStmt>)

  }

  /// The condition of the expression.
  public var condition: [SourceRepresentable<ConditionItem>]

  /// The body of the expression that's executed if the condition holds.
  public var success: Body

  /// The body of the expression that's executed if the condition does not hold.
  public var failure: Body?

}
