/// A for loop.
public struct ForStmt: Stmt, LexicalScope {

  /// The conditional binding of the loop.
  public let binding: NodeID<BindingDecl>

  /// The iteration domain of the loop.
  public let domain: AnyExprID

  /// The filter of the loop, if any.
  public let filter: AnyExprID?

  /// The body of the loop.
  public let body: NodeID<BraceStmt>

  internal init(
    binding: NodeID<BindingDecl>,
    domain: AnyExprID,
    filter: AnyExprID? = nil,
    body: NodeID<BraceStmt>
  ) {
    self.binding = binding
    self.domain = domain
    self.filter = filter
    self.body = body
  }

}
