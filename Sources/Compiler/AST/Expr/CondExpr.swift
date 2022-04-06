/// A conditional expression.
public struct CondExpr: Stmt {

  public enum Body {

    /// An expression body.
    case expr(Expr)

    /// A block body.
    case block(BraceStmt)

  }

  public var range: SourceRange?

  /// The condition of the expression.
  public var condition: [ConditionItem]

  /// The body of the expression that's executed if the condition holds.
  public var success: Body

  /// The body of the expression that's executed if the condition does not hold.
  public var failure: Body?

}
