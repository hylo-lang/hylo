/// A conditional expression.
public struct CondExpr: Hashable, ScopeOutliner {

  public enum Body: Hashable {

    /// An expression body.
    case expr(Expr)

    /// A block body.
    case block(BraceStmt)

  }

  var scopeID: ScopeID

  /// The condition of the expression.
  public var condition: [SourceRepresentable<ConditionItem>]

  /// The body of the expression that's executed if the condition holds.
  public var success: SourceRepresentable<Body>

  /// The body of the expression that's executed if the condition does not hold.
  public var failure: SourceRepresentable<Body>?

}
