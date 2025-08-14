/// A break statement.
public struct BreakStmt: Stmt, Sendable {

  public let site: SourceRange

  public init(site: SourceRange) {
    self.site = site
  }

}
