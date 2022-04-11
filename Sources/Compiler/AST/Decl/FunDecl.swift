/// A function declaration.
public struct FunDecl: GenericDecl, LexicalScope {

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
    case expr(AnyExprIndex)

    /// A block body.
    case block(NodeIndex<BraceStmt>)

    /// A method bundle.
    case bundle([NodeIndex<MethodImplDecl>])

  }

  /// The introducer of the declaration.
  public var introducer: SourceRepresentable<Introducer>

  /// The access modifier of the declaration, if any.
  public var accessModifier: SourceRepresentable<AccessModifier>?

  /// The member modifiers of the declaration.
  public var memberModifiers: [SourceRepresentable<MemberModifier>]

  /// The operator notation of the function.
  public var notation: SourceRepresentable<OperatorNotation>

  /// The identifier of the function, if any.
  public var identifier: SourceRepresentable<Identifier>?

  /// The generic clause of the function, if any.
  public var genericClause: SourceRepresentable<GenericClause>?

  /// The captures of the function.
  public var captures: [NodeIndex<BindingDecl>]

  /// The parameters of the function.
  public var parameters: [NodeIndex<ParamDecl>]

  /// The return type annotation of the function, if any.
  public var output: AnyTypeExprIndex?

  /// The body of the declaration, if any.
  public var body: SourceRepresentable<Body>?

}
