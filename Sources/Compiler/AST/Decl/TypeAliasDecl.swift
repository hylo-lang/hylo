/// A type alias declaration.
public struct TypeAliasDecl: Decl, SourceRepresentable {

  public enum Body {

    /// A single type expression.
    case typeExpr(TypeExpr)

    /// A union of product type declarations.
    case union(ProductTypeDecl)

  }

  public var range: SourceRange?

  /// The access modifier of the declaration, if any.
  public var access: AccessModifier?

  /// The identifier of the alias.
  public var identifier: Identifier

  /// The generic clause of the declaration, if any.
  public var genericClause: GenericClause?

  /// The member declarations in the lexical scope of the trait.
  public var members: [AnyDeclIndex]

  /// The body of the declaration.
  public var body: Body

}
