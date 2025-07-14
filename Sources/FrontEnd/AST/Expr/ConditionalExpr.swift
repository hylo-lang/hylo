/// A conditional expression.
public struct ConditionalExpr: Expr, LexicalScope, Sendable {

  public let site: SourceRange

  /// The site of the `if` keyword.
  public let introducerSite: SourceRange

  /// The condition of the expression.
  ///
  /// - Requires: `condition.count > 0`
  public let condition: [ConditionItem]

  /// The expression that's executed if the condition holds.
  public let success: AnyExprID

  /// The the expression that's executed if the condition does not hold.
  public let failure: Introduced<AnyExprID>

  public init(
    introducerSite: SourceRange,
    condition: [ConditionItem],
    success: AnyExprID,
    failure: Introduced<AnyExprID>,
    site: SourceRange
  ) {
    precondition(condition.count > 0)

    self.introducerSite = introducerSite
    self.site = site
    self.condition = condition
    self.success = success
    self.failure = failure
  }

}
