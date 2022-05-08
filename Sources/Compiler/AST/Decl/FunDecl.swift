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

  /// The captures of the function.
  public var captures: [NodeID<BindingDecl>]

  /// The parameters of the function.
  public var parameters: [NodeID<ParameterDecl>]

  /// The return type annotation of the function, if any.
  public var output: AnyTypeExprID?

  /// The body of the declaration, if any.
  public var body: SourceRepresentable<Body>?

  /// Indicates whether the declaration appears in an expression context.
  public var isInExprContext: Bool

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
    isInExprContext: Bool = false
  ) {
    self.introducer = introducer
    self.accessModifier = accessModifier
    self.memberModifiers = memberModifiers
    self.notation = notation
    self.identifier = identifier
    self.genericClause = genericClause
    self.captures = captures
    self.parameters = parameters
    self.output = output
    self.body = body
    self.isInExprContext = isInExprContext
  }

}
