/// A pattern which binds an identifier.
public struct NamePattern: Pattern {

  public let origin: SourceRange?

  /// The variable declaration introducing the pattern's name.
  public let decl: NodeID<VarDecl>

  public init(decl: NodeID<VarDecl>, origin: SourceRange?) {
    self.origin = origin
    self.decl = decl
  }

}
