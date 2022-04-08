/// A namespace declaration.
public struct NamespaceDecl: Decl, ScopeOutliner {

  var scopeID: ScopeID

  /// The access modifier of the declaration, if any.
  public var access: SourceRepresentable<AccessModifier>?

  /// The identifier of the namespace.
  public var identifier: SourceRepresentable<Identifier>

  /// The member declarations in the lexical scope of the namespace.
  public var members: [AnyDeclIndex]

  public var range: SourceRange?

}
