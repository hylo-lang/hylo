/// A pattern which binds an identifier.
public struct NamePattern: Pattern {

  /// The variable declaration introducing the pattern's name
  public var decl: NodeIndex<VarDecl>

}
