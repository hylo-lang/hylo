/// A while loop.
public struct WhileStmt: Stmt, LexicalScope {

  public static let kind = NodeKind.whileStmt

  /// The condition of the loop.
  ///
  /// - Requires `condition.count > 0`
  public var condition: [SourceRepresentable<ConditionItem>]

  /// The body of the loop.
  public var body: NodeID<BraceStmt>

  internal init(condition: [SourceRepresentable<ConditionItem>], body: NodeID<BraceStmt>) {
    precondition(condition.count > 0)
    self.condition = condition
    self.body = body
  }

}
