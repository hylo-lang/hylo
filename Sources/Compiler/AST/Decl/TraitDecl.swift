/// A trait declaration.
public struct TraitDecl: GenericDecl, LexicalScope {

  /// The access modifier of the declaration, if any.
  public var access: SourceRepresentable<AccessModifier>?

  /// The identifier of the trait.
  public var identifier: SourceRepresentable<Identifier>

  /// The (synthetic) generic clause of the declaration.
  public var genericClause: SourceRepresentable<GenericClause>?

  /// The names of traits which the trait refines.
  public var refinements: [NodeIndex<NameTypeExpr>]

  /// The member declarations in the lexical scope of the trait.
  public var members: [AnyDeclIndex]

}
