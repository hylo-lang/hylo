/// A namespace declaration.
public struct NamespaceDecl: SingleEntityDecl, LexicalScope {

  public static let kind = NodeKind.namespaceDecl

  /// The access modifier of the declaration, if any.
  public private(set) var accessModifier: SourceRepresentable<AccessModifier>?

  /// The identifier of the namespace.
  public let identifier: SourceRepresentable<Identifier>

  /// The member declarations in the lexical scope of the namespace.
  public let members: [AnyDeclID]

  /// Creates an instance with the given properties and no `accessModifier`.
  public init(
    identifier: SourceRepresentable<Identifier>,
    members: [AnyDeclID]
  ) {
    self.identifier = identifier
    self.members = members
  }

  public var name: String { identifier.value }

  /// Incorporates `accessModifier` into `self`.
  ///
  /// - Precondition: `self.accessModifier == nil`
  internal mutating func incorporate(_ accessModifier: SourceRepresentable<AccessModifier>) {
    precondition(self.accessModifier == nil)
    self.accessModifier = accessModifier
  }
}
