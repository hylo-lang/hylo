/// A conditional statement.
///
/// Unlike a `ConditionalExpr`, the branches of a conditional statement are represented by brace
/// statements. Further, a conditional statement might not have a failure branch.
public struct ConditionalStmt: Stmt, LexicalScope {

  public let site: SourceRange

  /// The site of the `if` introducer.
  public let introducerSite: SourceRange

  /// The condition of the expression.
  ///
  /// - Requires: `condition.count > 0`
  public let condition: [ConditionItem]

  /// The branch that's executed if the condition holds.
  public let success: BraceStmt.ID

  /// The branch that's executed if the condition does not hold.
  ///
  /// - Requires: `failure` is either a `ConditionalStmt`, a `BraceStmt`, or `nil`.
  public let failure: Introduced<AnyStmtID>?

  public init(
    introducerSite: SourceRange,
    condition: [ConditionItem],
    success: BraceStmt.ID,
    failure: Introduced<AnyStmtID>?,
    site: SourceRange
  ) {
    precondition(condition.count > 0)
    if let f = failure {
      precondition(f.value.kind == ConditionalStmt.self || f.value.kind == BraceStmt.self)
    }

    self.site = site
    self.introducerSite = introducerSite
    self.condition = condition
    self.success = success
    self.failure = failure
  }

}
