/// A for loop.
public struct ForStmt: Stmt, LexicalScope {
  public let site: SourceRange

  /// The site of the `for` introducer.
  public let introducerSite: SourceRange

  /// The conditional binding of the loop.
  public let binding: BindingDecl.ID

  /// The iteration domain of the loop.
  public let domain: Introduced<AnyExprID>

  /// The filter of the loop, if any.
  public let filter: Introduced<AnyExprID>?

  /// The body of the loop.
  public let body: BraceStmt.ID

  /// Creates an instance having the given properties.
  public init(
    introducerSite: SourceRange,
    binding: BindingDecl.ID,
    domain: Introduced<AnyExprID>,
    filter: Introduced<AnyExprID>?,
    body: BraceStmt.ID,
    site: SourceRange
  ) {
    self.site = site
    self.introducerSite = introducerSite
    self.binding = binding
    self.domain = domain
    self.filter = filter
    self.body = body
  }

}
