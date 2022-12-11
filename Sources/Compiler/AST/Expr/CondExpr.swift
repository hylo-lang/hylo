/// A conditional expression.
public struct CondExpr: Expr, LexicalScope {

  public enum Body: Codable {

    /// An expression body.
    case expr(AnyExprID)

    /// A block body.
    case block(NodeID<BraceStmt>)

  }

  public let origin: SourceRange?

  /// The condition of the expression.
  ///
  /// - Requires: `condition.count > 0`
  public let condition: [ConditionItem]

  /// The body of the expression that's executed if the condition holds.
  public let success: Body

  /// The body of the expression that's executed if the condition does not hold.
  public let failure: Body?

  public init(
    condition: [ConditionItem], success: CondExpr.Body, failure: CondExpr.Body?,
    origin: SourceRange?
  ) {
    precondition(condition.count > 0)

    self.origin = origin
    self.condition = condition
    self.success = success
    self.failure = failure
  }

}
