/// A conditional expression.
public struct CondExpr: Expr, ScopeOutliner {

  public enum Body {

    /// An expression body.
    case expr(Expr)

    /// A block body.
    case block(BraceStmt)

  }

  var scopeID: ScopeID

  public var range: SourceRange?

  /// The condition of the expression.
  public var condition: [ConditionItem]

  /// The body of the expression that's executed if the condition holds.
  public var success: Body

  /// The body of the expression that's executed if the condition does not hold.
  public var failure: Body?

  public func accept<V: ExprVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(cond: self)
  }

}
