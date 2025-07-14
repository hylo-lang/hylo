/// A brace statement (a.k.a. a code block).
public struct BraceStmt: Stmt, LexicalScope, Sendable {

  public let site: SourceRange

  /// The statements in the block.
  public let stmts: [AnyStmtID]

  public init(stmts: [AnyStmtID], site: SourceRange) {
    self.site = site
    self.stmts = stmts
  }

}
