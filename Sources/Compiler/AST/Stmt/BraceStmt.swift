/// A brace statement (a.k.a. a code block).
public struct BraceStmt: Stmt, LexicalScope {

  public static let kind = NodeKind.braceStmt

  /// The statements in the block.
  public let stmts: [AnyStmtID]

  public init(stmts: [AnyStmtID] = []) {
    self.stmts = stmts
  }

}
