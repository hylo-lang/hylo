/// A break statement.
public struct BreakStmt: Stmt {

  public let site: SourceRange

  public init(site: SourceRange) {
    self.site = site
  }

}
