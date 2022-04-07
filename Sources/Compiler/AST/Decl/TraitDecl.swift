/// A trait declaration.
public struct TraitDecl: Decl, ScopeOutliner, SourceRepresentable {

  var scopeID: ScopeID

  public var range: SourceRange?

  /// The access modifier of the declaration, if any.
  public var access: AccessModifier?

  /// The identifier of the trait.
  public var identifier: Identifier

  /// The names of traits which the trait refines.
  public var refinements: [NameTypeExpr]

  /// The member declarations in the lexical scope of the trait.
  public var members: [AnyDeclIndex]

}
