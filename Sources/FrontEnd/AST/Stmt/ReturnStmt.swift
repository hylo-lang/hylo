/// A return statement.
public struct ReturnStmt: Stmt {

  public let site: SourceRange

  /// The site of the `return` introducer.
  public let introducerSite: SourceRange

  /// The return value, if any.
  public let value: AnyExprID?

  public init(
    introducerSite: SourceRange,
    value: AnyExprID?,
    site: SourceRange
  ) {
    self.site = site
    self.introducerSite = introducerSite
    self.value = value
  }

}
