/// A (nominal) product type declaration.
public struct ProductTypeDecl: GenericDecl, SingleEntityDecl, LexicalScope {

  public static let kind = NodeKind.productTypeDecl

  /// The access modifier of the declaration, if any.
  public var access: SourceRepresentable<AccessModifier>?

  /// The identifier of the type.
  public var identifier: SourceRepresentable<Identifier>

  /// The generic clause of the declaration, if any.
  public var genericClause: SourceRepresentable<GenericClause>?

  /// The names of traits to which the type conforms.
  public var conformances: [NodeIndex<NameTypeExpr>]

  /// The member declarations in the lexical scope of the trait.
  public var members: [AnyDeclIndex]

  public var name: String { identifier.value }

}
