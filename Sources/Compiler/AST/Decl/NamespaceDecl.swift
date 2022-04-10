/// A namespace declaration.
public struct NamespaceDecl: SingleEntityDecl, LexicalScope {

  /// The access modifier of the declaration, if any.
  public var access: SourceRepresentable<AccessModifier>?

  /// The identifier of the namespace.
  public var identifier: SourceRepresentable<Identifier>

  /// The member declarations in the lexical scope of the namespace.
  public var members: [AnyDeclIndex]

  public var name: String { identifier.value }

}
