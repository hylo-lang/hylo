/// A declaration statement.
public struct DeclStmt: Stmt {

  public let site: SourceRange

  /// The declaration.
  public let decl: AnyDeclID

  public init(decl: AnyDeclID, site: SourceRange) {
    self.site = site
    self.decl = decl
  }

}
