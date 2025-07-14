/// A pattern which binds an identifier.
public struct NamePattern: Pattern, Sendable {

  public let site: SourceRange

  /// The variable declaration introducing the pattern's name.
  public let decl: VarDecl.ID

  public init(decl: VarDecl.ID, site: SourceRange) {
    self.site = site
    self.decl = decl
  }

}
