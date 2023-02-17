/// A conditional statement.
///
/// Unlike a `ConditionalExpr`, the branches of a conditional statement are represented by brace
/// statements. Further, a conditional statement might not have a failure branch.
public struct ConditionalStmt: Stmt, LexicalScope {

  public let site: SourceRange

  /// The condition of the expression.
  ///
  /// - Requires: `condition.count > 0`
  public let condition: [ConditionItem]

  /// The branch that's executed if the condition holds.
  public let success: NodeID<BraceStmt>

  /// The branch that's executed if the condition does not hold.
  ///
  /// - Requires: `failure` is either a `ConditionalStmt`, a `BraceStmt`, or `nil`.
  public let failure: AnyStmtID?

  public init(
    condition: [ConditionItem],
    success: NodeID<BraceStmt>,
    failure: AnyStmtID?,
    site: SourceRange
  ) {
    precondition(condition.count > 0)
    if let f = failure {
      precondition(f.kind == ConditionalStmt.self || f.kind == BraceStmt.self)
    }

    self.site = site
    self.condition = condition
    self.success = success
    self.failure = failure
  }

}
