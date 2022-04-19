/// A function declaration.
public struct FunDecl: GenericDecl, GenericScope {

  public static let kind = NodeKind.funDecl

  public enum Introducer: Hashable {

    /// The function and method introducer, `fun`.
    case fun

    /// The constructor introducer, `init`.
    case `init`

    /// The default constructor introducer, `default init`
    case defaultInit

    /// The destructor introducer, `deinit`.
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
  public var parameters: [NodeID<ParamDecl>]

  /// The return type annotation of the function, if any.
  public var output: AnyTypeExprID?

  /// The body of the declaration, if any.
  public var body: SourceRepresentable<Body>?

  public init(
    introducer: SourceRepresentable<Introducer>,
    accessModifier: SourceRepresentable<AccessModifier>? = nil,
    memberModifiers: [SourceRepresentable<MemberModifier>] = [],
    notation: SourceRepresentable<OperatorNotation>? = nil,
    identifier: SourceRepresentable<Identifier>? = nil,
    genericClause: SourceRepresentable<GenericClause>? = nil,
    captures: [NodeID<BindingDecl>] = [],
    parameters: [NodeID<ParamDecl>] = [],
    output: AnyTypeExprID? = nil,
    body: SourceRepresentable<Body>? = nil
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
  }

}
