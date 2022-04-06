/// A function declaration.
public struct FunDecl: Decl, SourceRepresentable {

  public struct Introducer: SourceRepresentable {

    public enum Kind {

      /// The function and method introducer, `fun`.
      case fun

      /// The constructor introducer, `init`.
      case `init`

      /// The default constructor introducer, `default init`
      case defaultInit

      /// The destructor introducer, `deinit`.
      case `deinit`

    }

    public var range: SourceRange?

    public var kind: Kind

  }

  public enum Body {

    /// An expression body.
    case expr(Expr)

    /// A block body.
    case block(BraceStmt)

    /// A method bundle.
    case bundle([DeclIndex<MethodImplDecl>])

  }

  public var range: SourceRange?

  /// The introducer of the declaration.
  public var introducer: Introducer

  /// The access modifier of the declaration, if any.
  public var accessModifier: AccessModifier?

  /// The member modifiers of the declaration.
  public var memberModifiers: [MemberModifier]

  /// The operator notation of the function.
  public var notation: OperatorNotation

  /// The identifier of the function, if any.
  public var identifier: Identifier?

  /// The generic clause of the function, if any.
  public var genericClause: GenericClause?

  /// The captures of the function.
  public var captures: [DeclIndex<BindingDecl>]

  /// The parameters of the function.
  public var parameters: [DeclIndex<ParamDecl>]

  /// The return type annotation of the function, if any.
  public var output: TypeExpr?

  /// The body of the declaration, if any.
  public var body: Body?

}
