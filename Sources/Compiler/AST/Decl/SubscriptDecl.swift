/// A subscript declaration.
public struct SubscriptDecl: GenericDecl, GenericScope {

  public static let kind = NodeKind.subscriptDecl

  public enum Introducer: Codable {

    /// The standard subscript introducer.
    case `subscript`

    /// The property introducer.
    case property

  }

  /// The introducer of the declaration.
  public var introducer: SourceRepresentable<Introducer>

  /// The attributes of the declaration, if any.
  public var attributes: [SourceRepresentable<Attribute>]

  /// The access modifier of the declaration, if any.
  public var accessModifier: SourceRepresentable<AccessModifier>?

  /// The member modifier of the declaration.
  public var memberModifier: SourceRepresentable<MemberModifier>?

  /// The receiver effect of the subscript.
  public var receiverEffect: SourceRepresentable<ReceiverEffect>?

  /// The identifier of the subscript, if any.
  public var identifier: SourceRepresentable<Identifier>?

  /// The generic clause of the subscript, if any.
  public var genericClause: SourceRepresentable<GenericClause>?

  /// The explicit capture declarations of the subscript.
  public var explicitCaptures: [NodeID<BindingDecl>]

  /// The parameters of the subscript, unless the declaration denotes a computed property.
  ///
  /// These declarations must have a type annotation.
  public var parameters: [NodeID<ParameterDecl>]?

  /// The output type annotation of the subscript.
  public var output: AnyTypeExprID

  /// The implementations of the subscript.
  public var impls: [NodeID<SubscriptImplDecl>]

  public init(
    introducer: SourceRepresentable<Introducer>,
    attributes: [SourceRepresentable<Attribute>] = [],
    accessModifier: SourceRepresentable<AccessModifier>? = nil,
    memberModifier: SourceRepresentable<MemberModifier>? = nil,
    receiverEffect: SourceRepresentable<ReceiverEffect>? = nil,
    identifier: SourceRepresentable<Identifier>? = nil,
    genericClause: SourceRepresentable<GenericClause>? = nil,
    explicitCaptures: [NodeID<BindingDecl>] = [],
    parameters: [NodeID<ParameterDecl>]? = nil,
    output: AnyTypeExprID,
    impls: [NodeID<SubscriptImplDecl>] = []
  ) {
    self.introducer = introducer
    self.attributes = attributes
    self.accessModifier = accessModifier
    self.memberModifier = memberModifier
    self.receiverEffect = receiverEffect
    self.identifier = identifier
    self.genericClause = genericClause
    self.explicitCaptures = explicitCaptures
    self.parameters = parameters
    self.output = output
    self.impls = impls
  }

  /// Returns whether the declaration is public.
  public var isPublic: Bool { accessModifier?.value == .public }

  /// Returns whether the declaration denotes a static subscript.
  public var isStatic: Bool { memberModifier?.value == .static }

  /// Returns whether the declaration denotes an `inout` subscript.
  public var isInout: Bool { receiverEffect?.value == .inout }

  /// Returns whether the declaration denotes a `sink` subscript.
  public var isSink: Bool { receiverEffect?.value == .sink }

}
