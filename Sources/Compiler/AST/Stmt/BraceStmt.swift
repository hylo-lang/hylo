/// A brace statement (a.k.a. a code block).
public struct BraceStmt: Stmt, LexicalScope {

  /// The statements in the block.
  public var stmts: [AnyStmtIndex]

}
