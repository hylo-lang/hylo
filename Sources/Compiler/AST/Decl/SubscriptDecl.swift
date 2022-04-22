/// A subscript declaration.
public struct SubscriptDecl: GenericDecl, GenericScope {

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

  /// The parameters of the subscript, unless the declaration denotes a computed property.
  public var parameters: [NodeID<ParameterDecl>]?

  /// The output type annotation of the subscript.
  public var output: AnyTypeExprID

  /// The implementations of the subscript.
  public var impls: [NodeID<SubscriptImplDecl>]

  public init(
    accessModifier: SourceRepresentable<AccessModifier>? = nil,
    memberModifiers: [SourceRepresentable<MemberModifier>] = [],
    identifier: SourceRepresentable<Identifier>? = nil,
    genericClause: SourceRepresentable<GenericClause>? = nil,
    captures: [NodeID<BindingDecl>] = [],
    parameters: [NodeID<ParameterDecl>]? = nil,
    output: AnyTypeExprID,
    impls: [NodeID<SubscriptImplDecl>]
  ) {
    self.accessModifier = accessModifier
    self.memberModifiers = memberModifiers
    self.identifier = identifier
    self.genericClause = genericClause
    self.captures = captures
    self.parameters = parameters
    self.output = output
    self.impls = impls
  }

}
