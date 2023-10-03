/// A yield statement.
public struct YieldStmt: Stmt {

  public let site: SourceRange

  /// The site of the `yield` introducer.
  public let introducerSite: SourceRange

  /// The yielded value.
  public let value: AnyExprID

  public init(introducerSite: SourceRange, value: AnyExprID, site: SourceRange) {
    self.site = site
    self.introducerSite = introducerSite
    self.value = value
  }

}
