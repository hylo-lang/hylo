/// A type alias declaration.
public struct TypeAliasDecl: SingleEntityDecl, GenericDecl, TypeScope, GenericScope {

  public enum Body: Codable {

    /// A single type expression.
    case typeExpr(AnyTypeExprID)

    /// A union of product type declarations.
    case union([NodeID<ProductTypeDecl>])

  }

  public let origin: SourceRange

  /// The access modifier of the declaration, if any.
  public let accessModifier: SourceRepresentable<AccessModifier>?

  /// The identifier of the alias.
  public let identifier: SourceRepresentable<Identifier>

  /// The generic clause of the declaration, if any.
  public let genericClause: SourceRepresentable<GenericClause>?

  /// The body of the declaration.
  public let body: Body

  /// Creates an instance with the given properties.
  public init(
    accessModifier: SourceRepresentable<AccessModifier>?,
    identifier: SourceRepresentable<Identifier>,
    genericClause: SourceRepresentable<GenericClause>?,
    body: Body,
    origin: SourceRange
  ) {
    self.origin = origin
    self.accessModifier = accessModifier
    self.identifier = identifier
    self.genericClause = genericClause
    self.body = body
  }

  public var name: String { identifier.value }

}
