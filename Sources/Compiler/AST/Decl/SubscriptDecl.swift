/// A subscript declaration.
public struct SubscriptDecl: Decl, SourceRepresentable {

  public var range: SourceRange?

  /// The access modifier of the declaration, if any.
  public var accessModifier: AccessModifier?

  /// The member modifiers of the declaration.
  public var memberModifiers: [MemberModifier]

  /// The identifier of the subscript, if any.
  public var identifier: Identifier?

  /// The generic clause of the subscript, if any.
  public var genericClause: GenericClause?

  /// The captures of the subscript.
  public var captures: [DeclIndex<BindingDecl>]

  /// The parameters of the subscript.
  public var parameters: [DeclIndex<ParamDecl>]

  /// The output type annotation of the subscript.
  public var output: TypeExpr?

  /// The implementations of the subscript.
  public var implementations: [DeclIndex<SubscriptImplDecl>]

}
