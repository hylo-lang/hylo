/// A namespace declaration.
public struct NamespaceDecl: SingleEntityDecl, LexicalScope {

  public static let kind = NodeKind.namespaceDecl

  /// The access modifier of the declaration, if any.
  public let accessModifier: SourceRepresentable<AccessModifier>?

  /// The identifier of the namespace.
  public let identifier: SourceRepresentable<Identifier>

  /// The member declarations in the lexical scope of the namespace.
  public let members: [AnyDeclID]

  /// Creates an instance with the given properties.
  public init(
    accessModifier: SourceRepresentable<AccessModifier>?,
    identifier: SourceRepresentable<Identifier>,
    members: [AnyDeclID]
  ) {
    self.accessModifier = accessModifier
    self.identifier = identifier
    self.members = members
  }

  public var name: String { identifier.value }

}
