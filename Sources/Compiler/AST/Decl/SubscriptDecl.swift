/// A subscript declaration.
public struct SubscriptDecl: GenericDecl, GenericScope {

  public static let kind = NodeKind.subscriptDecl

  public enum Body: Hashable {

    /// An expression body.
    case expr(AnyExprID)

    /// A block body.
    case block(NodeID<BraceStmt>)

    /// A bundle.
    case bundle([NodeID<SubscriptImplDecl>])

  }

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
  public var parameters: [NodeID<ParameterDecl>]?

  /// The output type annotation of the subscript.
  public var output: AnyTypeExprID

  /// The body of the declaration, if any.
  public var body: Body?

  public init(
    accessModifier: SourceRepresentable<AccessModifier>? = nil,
    memberModifier: SourceRepresentable<MemberModifier>? = nil,
    receiverEffect: SourceRepresentable<ReceiverEffect>? = nil,
    identifier: SourceRepresentable<Identifier>? = nil,
    genericClause: SourceRepresentable<GenericClause>? = nil,
    explicitCaptures: [NodeID<BindingDecl>] = [],
    parameters: [NodeID<ParameterDecl>]? = nil,
    output: AnyTypeExprID,
    body: Body? = nil
  ) {
    self.accessModifier = accessModifier
    self.memberModifier = memberModifier
    self.receiverEffect = receiverEffect
    self.identifier = identifier
    self.genericClause = genericClause
    self.explicitCaptures = explicitCaptures
    self.parameters = parameters
    self.output = output
    self.body = body
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
