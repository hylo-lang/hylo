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
  public let introducer: SourceRepresentable<Introducer>

  /// The attributes of the declaration, if any.
  public let attributes: [SourceRepresentable<Attribute>]

  /// The access modifier of the declaration, if any.
  public let accessModifier: SourceRepresentable<AccessModifier>?

  /// The member modifier of the declaration.
  public let memberModifier: SourceRepresentable<MemberModifier>?

  /// The receiver effect of the subscript.
  public let receiverEffect: SourceRepresentable<ReceiverEffect>?

  /// The identifier of the subscript, if any.
  public let identifier: SourceRepresentable<Identifier>?

  /// The generic clause of the subscript, if any.
  public let genericClause: SourceRepresentable<GenericClause>?

  /// The explicit capture declarations of the subscript.
  public let explicitCaptures: [NodeID<BindingDecl>]

  /// The parameters of the subscript, unless the declaration denotes a computed property.
  ///
  /// These declarations must have a type annotation.
  public let parameters: [NodeID<ParameterDecl>]?

  /// The output type annotation of the subscript.
  public let output: AnyTypeExprID

  /// The implementations of the subscript.
  public let impls: [NodeID<SubscriptImplDecl>]

  /// The declaration of the implicit parameters of the subscript, if any.
  ///
  /// This property is set during type checking. It maps the names of the implicit and explicit
  /// captures to their respective declaration.
  ///
  /// Note that the implicit receiver parameter (i.e., `self`) of a subscipt is never stored in
  /// this property. Each subscript implementation declaration has its own declaration.
  public private(set) var implicitParameterDecls: [ImplicitParameter] = []

  /// Creates an instance with the given properties.
  public init(
    introducer: SourceRepresentable<Introducer>,
    attributes: [SourceRepresentable<Attribute>],
    accessModifier: SourceRepresentable<AccessModifier>?,
    memberModifier: SourceRepresentable<MemberModifier>?,
    receiverEffect: SourceRepresentable<ReceiverEffect>?,
    identifier: SourceRepresentable<Identifier>?,
    genericClause: SourceRepresentable<GenericClause>?,
    explicitCaptures: [NodeID<BindingDecl>],
    parameters: [NodeID<ParameterDecl>]?,
    output: AnyTypeExprID,
    impls: [NodeID<SubscriptImplDecl>]
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

  /// Incorporates the given implicit parameter declarations into `self`.
  ///
  /// - Requires: `self.implicitParameterDecls` is empty.
  internal mutating func incorporate(implicitParameterDecls: [ImplicitParameter]) {
    precondition(self.implicitParameterDecls.isEmpty)
    self.implicitParameterDecls = implicitParameterDecls
  }

}
