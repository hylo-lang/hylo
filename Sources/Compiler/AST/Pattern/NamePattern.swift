/// A pattern which binds an identifier.
public struct NamePattern: Hashable {

  /// The variable declaration introducing the pattern's name
  public var decl: DeclIndex<VarDecl>

}
