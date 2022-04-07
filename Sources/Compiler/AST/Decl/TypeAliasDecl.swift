/// A type alias declaration.
public struct TypeAliasDecl: Decl, ScopeOutliner, SourceRepresentable {

  public enum Body {

    /// A single type expression.
    case typeExpr(TypeExpr)

    /// A union of product type declarations.
    case union([DeclIndex<ProductTypeDecl>])

  }

  var scopeID: ScopeID

  public var range: SourceRange?

  /// The access modifier of the declaration, if any.
  public var access: AccessModifier?

  /// The identifier of the alias.
  public var identifier: Identifier

  /// The generic clause of the declaration, if any.
  public var genericClause: GenericClause?

  /// The body of the declaration.
  public var body: Body

  public func accept<V: DeclVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(typeAlias: self)
  }

}
