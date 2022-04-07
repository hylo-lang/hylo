/// A namespace declaration.
public struct NamespaceDecl: Decl, ScopeOutliner, SourceRepresentable {

  var scopeID: ScopeID

  public var range: SourceRange?

  /// The access modifier of the declaration, if any.
  public var access: AccessModifier?

  /// The identifier of the namespace.
  public var identifier: Identifier

  /// The member declarations in the lexical scope of the namespace.
  public var members: [AnyDeclIndex]

}
