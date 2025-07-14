/// A continue statement.
public struct ContinueStmt: Stmt, Sendable {

  public let site: SourceRange

  public init(site: SourceRange) {
    self.site = site
  }

}
