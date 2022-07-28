/// A function declaration.
public struct FunDecl: GenericDecl, GenericScope {

  public static let kind = NodeKind.funDecl

  public enum Introducer: Codable {

    /// The function and method introducer, `fun`.
    case fun

    /// The initializer introducer, `init`.
    case `init`

    /// The memberwise initializer introducer, `memberwise init`
    case memberwiseInit

    /// The deinitializer introducer, `deinit`.
    case `deinit`

  }

  public enum Body: Codable {

    /// An expression body.
    case expr(AnyExprID)

    /// A block body.
    case block(NodeID<BraceStmt>)

    /// A bundle.
    case bundle([NodeID<MethodImplDecl>])

  }

  public struct ImplicitParameter: Codable {

    let name: String

    var decl: AnyDeclID

  }

  /// The introducer of the declaration.
  public var introducer: SourceRepresentable<Introducer>

  /// The attributes of the declaration, if any.
  public var attributes: [SourceRepresentable<Attribute>]

  /// The access modifier of the declaration, if any.
  public var accessModifier: SourceRepresentable<AccessModifier>?

  /// The member modifier of the declaration.
  public var memberModifier: SourceRepresentable<MemberModifier>?

  /// The receiver effect of the function.
  public var receiverEffect: SourceRepresentable<ReceiverEffect>?

  /// The operator notation of the function.
  public var notation: SourceRepresentable<OperatorNotation>?

  /// The identifier of the function, if any.
  public var identifier: SourceRepresentable<Identifier>?

  /// The generic clause of the function, if any.
  public var genericClause: SourceRepresentable<GenericClause>?

  /// The explicit capture declarations of the function.
  public var explicitCaptures: [NodeID<BindingDecl>]

  /// The parameters of the function.
  public var parameters: [NodeID<ParameterDecl>]

  /// The return type annotation of the function, if any.
  public var output: AnyTypeExprID?

  /// The body of the declaration, if any.
  public var body: Body?

  /// Indicates whether the declaration appears in an expression context.
  public var isInExprContext: Bool

  /// The declaration of the implicit parameters of the function, if any.
  ///
  /// This property is set during type checking. In a local function, it maps the names of the
  /// implicit and explicit captures to their respective declaration. In a non-static method
  /// declaration, it contains a reference to the declaration of the implicit receiver parameter
  /// (i.e., `self`), unless the method forms a bundle. The implicit receiver of a method bundle
  /// has a declaration in each method implementation.
  public internal(set) var implicitParameterDecls: [ImplicitParameter] = []

  /// The declaration of the implicit receiver parameter, if any.
  public var implicitReceiverDecl: NodeID<ParameterDecl>? {
    if let parameter = implicitParameterDecls.first, parameter.name == "self" {
      return NodeID(converting: parameter.decl)
    } else {
      return nil
    }
  }

  public init(
    introducer: SourceRepresentable<Introducer>,
    attributes: [SourceRepresentable<Attribute>] = [],
    accessModifier: SourceRepresentable<AccessModifier>? = nil,
    memberModifier: SourceRepresentable<MemberModifier>? = nil,
    receiverEffect: SourceRepresentable<ReceiverEffect>? = nil,
    notation: SourceRepresentable<OperatorNotation>? = nil,
    identifier: SourceRepresentable<Identifier>? = nil,
    genericClause: SourceRepresentable<GenericClause>? = nil,
    captures: [NodeID<BindingDecl>] = [],
    parameters: [NodeID<ParameterDecl>] = [],
    output: AnyTypeExprID? = nil,
    body: Body? = nil,
    receiver: NodeID<ParameterDecl>? = nil,
    isInExprContext: Bool = false
  ) {
    self.introducer = introducer
    self.attributes = attributes
    self.accessModifier = accessModifier
    self.memberModifier = memberModifier
    self.receiverEffect = receiverEffect
    self.notation = notation
    self.identifier = identifier
    self.genericClause = genericClause
    self.explicitCaptures = captures
    self.parameters = parameters
    self.output = output
    self.body = body
    self.isInExprContext = isInExprContext
  }

  /// Returns whether the declaration is public.
  public var isPublic: Bool { accessModifier?.value == .public }

  /// Returns whether the declaration denotes a static method.
  public var isStatic: Bool { memberModifier?.value == .static }

  /// Returns whether the declaration denotes an `inout` method.
  public var isInout: Bool { receiverEffect?.value == .inout }

  /// Returns whether the declaration denotes a `sink` method.
  public var isSink: Bool { receiverEffect?.value == .sink }

  /// Returns whether the declaration denotes a method bundle.
  public var isBundle: Bool {
    if case .bundle = body {
      return true
    } else {
      return false
    }
  }

}
