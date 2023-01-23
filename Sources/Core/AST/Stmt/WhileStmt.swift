/// A while loop.
public struct WhileStmt: Stmt, LexicalScope {

  public let site: SourceRange

  /// The condition of the loop.
  ///
  /// - Requires `condition.count > 0`
  public let condition: [ConditionItem]

  /// The body of the loop.
  public let body: NodeID<BraceStmt>

  public init(condition: [ConditionItem], body: NodeID<BraceStmt>, site: SourceRange) {
    precondition(condition.count > 0)

    self.site = site
    self.condition = condition
    self.body = body
  }

}
