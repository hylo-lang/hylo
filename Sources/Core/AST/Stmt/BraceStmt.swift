/// A brace statement (a.k.a. a code block).
public struct BraceStmt: Stmt, LexicalScope {

  public let origin: SourceRange

  /// The statements in the block.
  public let stmts: [AnyStmtID]

  public init(stmts: [AnyStmtID], origin: SourceRange) {
    self.origin = origin
    self.stmts = stmts
  }

}
