/// A while loop.
public struct WhileStmt: Stmt, LexicalScope {

  public static let kind = NodeKind.whileStmt

  /// The condition of the loop.
  public var condition: [SourceRepresentable<ConditionItem>]

  /// The body of the loop.
  public var body: NodeIndex<BraceStmt>

}
