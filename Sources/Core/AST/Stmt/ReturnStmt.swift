/// A return statement.
public struct ReturnStmt: Stmt {

  public let site: SourceRange

  /// The return value, if any.
  public let value: AnyExprID?

  public init(value: AnyExprID?, site: SourceRange) {
    self.site = site
    self.value = value
  }

}
