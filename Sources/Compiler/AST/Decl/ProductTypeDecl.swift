/// A (nominal) product type declaration.
public struct ProductTypeDecl: GenericDecl, SingleEntityDecl, GenericScope {

  public static let kind = NodeKind.productTypeDecl

  /// The access modifier of the declaration, if any.
  public var accessModifier: SourceRepresentable<AccessModifier>?

  /// The identifier of the type.
  public let identifier: SourceRepresentable<Identifier>

  /// The generic clause of the declaration, if any.
  public let genericClause: SourceRepresentable<GenericClause>?

  /// The names of traits to which the type conforms.
  public let conformances: [NodeID<NameTypeExpr>]

  /// The member declarations in the lexical scope of the trait.
  public var members: [AnyDeclID]

  public init(
    accessModifier: SourceRepresentable<AccessModifier>? = nil,
    identifier: SourceRepresentable<Identifier>,
    genericClause: SourceRepresentable<GenericClause>? = nil,
    conformances: [NodeID<NameTypeExpr>] = [],
    members: [AnyDeclID] = []
  ) {
    self.accessModifier = accessModifier
    self.identifier = identifier
    self.genericClause = genericClause
    self.conformances = conformances
    self.members = members
  }

  public var name: String { identifier.value }

  /// Returns whether the declaration is public.
  public var isPublic: Bool { accessModifier?.value != nil }

}
