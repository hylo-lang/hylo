/// A subscript declaration.
public struct SubscriptDecl: Decl, ScopeOutliner {

  var scopeID: ScopeID

  /// The access modifier of the declaration, if any.
  public var accessModifier: SourceRepresentable<AccessModifier>?

  /// The member modifiers of the declaration.
  public var memberModifiers: [SourceRepresentable<MemberModifier>]

  /// The identifier of the subscript, if any.
  public var identifier: SourceRepresentable<Identifier>?

  /// The generic clause of the subscript, if any.
  public var genericClause: SourceRepresentable<GenericClause>?

  /// The captures of the subscript.
  public var captures: [DeclIndex<BindingDecl>]

  /// The parameters of the subscript.
  public var parameters: [DeclIndex<ParamDecl>]

  /// The output type annotation of the subscript.
  public var output: SourceRepresentable<TypeExpr>

  /// The implementations of the subscript.
  public var impls: [DeclIndex<SubscriptImplDecl>]

  public var range: SourceRange?

}
