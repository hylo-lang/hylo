/// A do-while loop.
public struct DoWhileStmt: Stmt {

  public static let kind = NodeKind.doWhileStmt

  /// The body of the loop.
  public var body: NodeIndex<BraceStmt>

  /// The condition of the loop.
  ///
  /// - Note: The condition is evaluated in the lexical scope of the body.
  public var condition: AnyExprIndex

}
