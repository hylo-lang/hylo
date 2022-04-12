/// A subscript declaration.
public struct SubscriptDecl: Decl, LexicalScope {

  public static let kind = NodeKind.subscriptDecl

  /// The access modifier of the declaration, if any.
  public var accessModifier: SourceRepresentable<AccessModifier>?

  /// The member modifiers of the declaration.
  public var memberModifiers: [SourceRepresentable<MemberModifier>]

  /// The identifier of the subscript, if any.
  public var identifier: SourceRepresentable<Identifier>?

  /// The generic clause of the subscript, if any.
  public var genericClause: SourceRepresentable<GenericClause>?

  /// The captures of the subscript.
  public var captures: [NodeID<BindingDecl>]

  /// The parameters of the subscript.
  public var parameters: [NodeID<ParamDecl>]

  /// The output type annotation of the subscript.
  public var output: AnyTypeExprID

  /// The implementations of the subscript.
  public var impls: [NodeID<SubscriptImplDecl>]

}
