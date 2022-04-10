/// A for loop.
public struct ForStmt: Stmt, LexicalScope {

  /// The conditional binding of the loop.
  public var binding: NodeIndex<BindingDecl>

  /// The iteration domain of the loop.
  public var domain: AnyExprIndex

  /// The filter of the loop, if any.
  public var filter: AnyExprIndex?

  /// The body of the loop.
  public var body: NodeIndex<BraceStmt>

}
