/// A pattern which binds an identifier.
public struct NamePattern: Pattern {

  public var range: SourceRange?

  /// The variable declaration introducing the pattern's name
  public var decl: DeclIndex<VarDecl>

}
