/// A namespace declaration.
public struct NamespaceDecl: SingleEntityDecl, LexicalScope {

  public static let kind = NodeKind.namespaceDecl

  /// The access modifier of the declaration, if any.
  public var access: SourceRepresentable<AccessModifier>?

  /// The identifier of the namespace.
  public var identifier: SourceRepresentable<Identifier>

  /// The member declarations in the lexical scope of the namespace.
  public var members: [AnyDeclID]

  public init(
    access: SourceRepresentable<AccessModifier>? = nil,
    identifier: SourceRepresentable<Identifier>,
    members: [AnyDeclID] = []
  ) {
    self.access = access
    self.identifier = identifier
    self.members = members
  }

  public var name: String { identifier.value }

}
