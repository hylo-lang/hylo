/// A pattern which binds an identifier.
public struct NamePattern: Pattern {

  public let site: SourceRange

  /// The variable declaration introducing the pattern's name.
  public let decl: NodeID<VarDecl>

  public init(decl: NodeID<VarDecl>, site: SourceRange) {
    self.site = site
    self.decl = decl
  }

}
