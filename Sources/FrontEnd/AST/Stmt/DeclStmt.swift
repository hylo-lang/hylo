/// A declaration statement.
public struct DeclStmt: Stmt, Sendable {

  public let site: SourceRange

  /// The declaration.
  public let decl: AnyDeclID

  public init(decl: AnyDeclID, site: SourceRange) {
    self.site = site
    self.decl = decl
  }

}
