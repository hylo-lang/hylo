/// A function declaration.
public struct FunDecl: GenericDecl, GenericScope {

  public static let kind = NodeKind.funDecl

  public enum Introducer: Hashable {

    /// The function and method introducer, `fun`.
    case fun

    /// The initializer introducer, `init`.
    case `init`

    /// The memberwise initializer introducer, `memberwise init`
    case memberwiseInit

    /// The deinitializer introducer, `deinit`.
    case `deinit`

  }

  public enum Body: Hashable {

    /// An expression body.
    case expr(AnyExprID)

    /// A block body.
    case block(NodeID<BraceStmt>)

    /// A method bundle.
    case bundle([NodeID<MethodImplDecl>])

  }

  /// The introducer of the declaration.
  public var introducer: SourceRepresentable<Introducer>

  /// The access modifier of the declaration, if any.
  public var accessModifier: SourceRepresentable<AccessModifier>?

  /// The member modifiers of the declaration.
  public var memberModifiers: [SourceRepresentable<MemberModifier>]

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
  public var body: SourceRepresentable<Body>?

  /// Indicates whether the declaration appears in an expression context.
  public var isInExprContext: Bool

  /// The declaration of the implicit parameters of the function, if any.
  ///
  /// This property is set during type checking. In a local function, it maps the names of the
  /// implicit and explicit captures to their respective declaration.In a non-static method
  /// declaration, it contains a reference to the declaration of the implicit receiver parameter
  /// (i.e., `self`), unless the method forms a bundle. The implicit receiver of a method bundle
  /// has a declaration in each method implementation.
  public internal(set) var implicitParameterDecls: [(name: String, decl: AnyDeclID)] = []

  /// The declaration of the implicit receiver parameter, if any.
  public var implicitReceiverDecl: NodeID<ParameterDecl>? {
    if let (name, decl) = implicitParameterDecls.first, name == "self" {
      return NodeID(converting: decl)
    } else {
      return nil
    }
  }

  public init(
    introducer: SourceRepresentable<Introducer>,
    accessModifier: SourceRepresentable<AccessModifier>? = nil,
    memberModifiers: [SourceRepresentable<MemberModifier>] = [],
    notation: SourceRepresentable<OperatorNotation>? = nil,
    identifier: SourceRepresentable<Identifier>? = nil,
    genericClause: SourceRepresentable<GenericClause>? = nil,
    captures: [NodeID<BindingDecl>] = [],
    parameters: [NodeID<ParameterDecl>] = [],
    output: AnyTypeExprID? = nil,
    body: SourceRepresentable<Body>? = nil,
    receiver: NodeID<ParameterDecl>? = nil,
    isInExprContext: Bool = false
  ) {
    self.introducer = introducer
    self.accessModifier = accessModifier
    self.memberModifiers = memberModifiers
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
  public var isPublic: Bool { accessModifier?.value != nil }

  /// Returns whether the declaration denotes a static method.
  public var isStatic: Bool { memberModifiers.contains(where: { $0.value == .static }) }

  /// Returns whether the declaration denotes an `inout` method.
  public var isInout: Bool { memberModifiers.contains(where: { $0.value == .receiver(.inout) }) }

  /// Returns whether the declaration denotes a `sink` method.
  public var isSink: Bool { memberModifiers.contains(where: { $0.value == .receiver(.sink) }) }

  /// Returns whether the declaration denotes a method bundler.
  public var isBundle: Bool {
    if case .bundle = body?.value {
      return true
    } else {
      return false
    }
  }

}
