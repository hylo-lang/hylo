/// A type alias declaration.
public struct TypeAliasDecl: GenericDecl, SingleEntityDecl, GenericScope {

  public static let kind = NodeKind.typeAliasDecl

  public enum Body: Hashable {

    /// A single type expression.
    case typeExpr(AnyTypeExprID)

    /// A union of product type declarations.
    case union([NodeID<ProductTypeDecl>])

  }

  /// The access modifier of the declaration, if any.
  public var accessModifier: SourceRepresentable<AccessModifier>?

  /// The identifier of the alias.
  public var identifier: SourceRepresentable<Identifier>

  /// The generic clause of the declaration, if any.
  public var genericClause: SourceRepresentable<GenericClause>?

  /// The body of the declaration.
  public var body: Body

  public init(
    accessModifier: SourceRepresentable<AccessModifier>? = nil,
    identifier: SourceRepresentable<Identifier>,
    genericClause: SourceRepresentable<GenericClause>? = nil,
    body: Body
  ) {
    self.accessModifier = accessModifier
    self.identifier = identifier
    self.genericClause = genericClause
    self.body = body
  }

  public var name: String { identifier.value }

}
