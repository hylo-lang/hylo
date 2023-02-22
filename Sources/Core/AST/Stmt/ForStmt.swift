/// A for loop.
public struct ForStmt: Stmt, LexicalScope {

  public let site: SourceRange

  /// The conditional binding of the loop.
  public let binding: BindingDecl.ID

  /// The iteration domain of the loop.
  public let domain: AnyExprID

  /// The filter of the loop, if any.
  public let filter: AnyExprID?

  /// The body of the loop.
  public let body: BraceStmt.ID

  /// Creates an instance having the given properties.
  public init(
    binding: BindingDecl.ID,
    domain: AnyExprID,
    filter: AnyExprID?,
    body: BraceStmt.ID,
    site: SourceRange
  ) {
    self.site = site
    self.binding = binding
    self.domain = domain
    self.filter = filter
    self.body = body
  }

}
