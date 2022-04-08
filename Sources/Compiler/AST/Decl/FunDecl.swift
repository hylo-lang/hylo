/// A function declaration.
public struct FunDecl: GenericDecl, ScopeOutliner {

  public enum Introducer {

    /// The function and method introducer, `fun`.
    case fun

    /// The constructor introducer, `init`.
    case `init`

    /// The default constructor introducer, `default init`
    case defaultInit

    /// The destructor introducer, `deinit`.
    case `deinit`

  }

  public enum Body {

    /// An expression body.
    case expr(Expr)

    /// A block body.
    case block(BraceStmt)

    /// A method bundle.
    case bundle([DeclIndex<MethodImplDecl>])

  }

  var scopeID: ScopeID

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
  public var captures: [DeclIndex<BindingDecl>]

  /// The parameters of the function.
  public var parameters: [DeclIndex<ParamDecl>]

  /// The return type annotation of the function, if any.
  public var output: SourceRepresentable<TypeExpr>?

  /// The body of the declaration, if any.
  public var body: SourceRepresentable<Body>?

  public var range: SourceRange?

}
