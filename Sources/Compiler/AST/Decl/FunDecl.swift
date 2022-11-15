/// A function declaration.
public struct FunDecl: GenericDecl, GenericScope {

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

  /// The introducer of the declaration.
  public let introducer: SourceRepresentable<Introducer>

  /// The attributes of the declaration, if any.
  public private(set) var attributes: [SourceRepresentable<Attribute>]

  /// The access modifier of the declaration, if any.
  public private(set) var accessModifier: SourceRepresentable<AccessModifier>?

  /// The member modifier of the declaration.
  public private(set) var memberModifier: SourceRepresentable<MemberModifier>?

  /// The receiver effect of the function.
  public let receiverEffect: SourceRepresentable<ReceiverEffect>?

  /// The operator notation of the function.
  public let notation: SourceRepresentable<OperatorNotation>?

  /// The identifier of the function, if any.
  public let identifier: SourceRepresentable<Identifier>?

  /// The generic clause of the function, if any.
  public let genericClause: SourceRepresentable<GenericClause>?

  /// The explicit capture declarations of the function.
  public let explicitCaptures: [NodeID<BindingDecl>]

  /// The parameters of the function.
  ///
  /// These declarations must have a type annotation unless `self.isInExprContext` is `true`.
  public let parameters: [NodeID<ParameterDecl>]

  /// The return type annotation of the function, if any.
  public let output: AnyTypeExprID?

  /// The body of the declaration, if any.
  public let body: Body?

  /// Indicates whether the declaration appears in an expression context.
  public let isInExprContext: Bool

  /// The declaration of the implicit parameters of the function, if any.
  ///
  /// This property is set during type checking. In a local function, it maps the names of the
  /// implicit and explicit captures to their respective declaration. In a non-static method
  /// declaration, it contains a reference to the declaration of the implicit receiver parameter
  /// (i.e., `self`), unless the method forms a bundle. The implicit receiver of a bundle has a
  /// declaration in each method implementation.
  public private(set) var implicitParameterDecls: [ImplicitParameter]

  /// The declaration of the implicit receiver parameter, if any.
  public var implicitReceiverDecl: NodeID<ParameterDecl>? {
    if let parameter = implicitParameterDecls.first, parameter.name == "self" {
      return NodeID(parameter.decl)
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
    receiver: NodeID<ParameterDecl>? = nil,
    output: AnyTypeExprID? = nil,
    body: Body? = nil,
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

    if let r = receiver {
      implicitParameterDecls = [ImplicitParameter(name: "self", decl: AnyDeclID(r))]
    } else {
      precondition(introducer.value != .`init`)
      precondition(introducer.value != .memberwiseInit)
      precondition(introducer.value != .deinit)
      implicitParameterDecls = []
    }
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

  /// Incorporates the given decorations into `self`.
  ///
  /// - Requires: `self` is undecorated.
  internal mutating func incorporate(
    attributes: [SourceRepresentable<Attribute>],
    accessModifier: SourceRepresentable<AccessModifier>?,
    memberModifier: SourceRepresentable<MemberModifier>?
  ) {
    precondition(self.attributes.isEmpty)
    precondition(self.accessModifier == nil)
    precondition(self.memberModifier == nil)
    self.attributes = attributes
    self.accessModifier = accessModifier
    self.memberModifier = memberModifier
  }

  /// Incorporates the given implicit parameter declarations into `self`.
  ///
  /// - Requires: `self.implicitParameterDecls` is empty.
  internal mutating func incorporate(implicitParameterDecls: [ImplicitParameter]) {
    precondition(self.implicitParameterDecls.isEmpty)
    self.implicitParameterDecls = implicitParameterDecls
  }

}
