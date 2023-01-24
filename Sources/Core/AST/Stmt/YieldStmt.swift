/// A yield statement.
public struct YieldStmt: Stmt {

  public let site: SourceRange

  /// The yielded value.
  public let value: AnyExprID

  public init(value: AnyExprID, site: SourceRange) {
    self.site = site
    self.value = value
  }

}
