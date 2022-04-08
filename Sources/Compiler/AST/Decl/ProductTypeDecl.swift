/// A (nominal) product type declaration.
public struct ProductTypeDecl: GenericDecl, ScopeOutliner {

  var scopeID: ScopeID

  /// The access modifier of the declaration, if any.
  public var access: SourceRepresentable<AccessModifier>?

  /// The identifier of the type.
  public var identifier: SourceRepresentable<Identifier>

  /// The generic clause of the declaration, if any.
  public var genericClause: SourceRepresentable<GenericClause>?

  /// The names of traits to which the type conforms.
  public var conformances: [SourceRepresentable<NameTypeExpr>]

  /// The member declarations in the lexical scope of the trait.
  public var members: [AnyDeclIndex]

  public var range: SourceRange?

}
