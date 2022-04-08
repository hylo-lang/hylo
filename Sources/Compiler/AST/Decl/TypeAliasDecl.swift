/// A type alias declaration.
public struct TypeAliasDecl: GenericDecl, ScopeOutliner {

  public enum Body {

    /// A single type expression.
    case typeExpr(TypeExpr)

    /// A union of product type declarations.
    case union([DeclIndex<ProductTypeDecl>])

  }

  var scopeID: ScopeID

  /// The access modifier of the declaration, if any.
  public var access: SourceRepresentable<AccessModifier>?

  /// The identifier of the alias.
  public var identifier: SourceRepresentable<Identifier>

  /// The generic clause of the declaration, if any.
  public var genericClause: SourceRepresentable<GenericClause>?

  /// The body of the declaration.
  public var body: SourceRepresentable<Body>

  public var range: SourceRange?

}
