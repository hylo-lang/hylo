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
  public var access: SourceRepresentable<AccessModifier>?

  /// The identifier of the alias.
  public var identifier: SourceRepresentable<Identifier>

  /// The generic clause of the declaration, if any.
  public var genericClause: SourceRepresentable<GenericClause>?

  /// The body of the declaration.
  public var body: SourceRepresentable<Body>

  public var name: String { identifier.value }

}
