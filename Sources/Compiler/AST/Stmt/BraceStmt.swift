/// A brace statement (a.k.a. a code block).
public struct BraceStmt: ScopeOutliner, Hashable {

  var scopeID: ScopeID

  /// The statements in the block.
  public var stmts: [SourceRepresentable<Stmt>]

}
