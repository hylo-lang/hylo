/// The declaration of a method implementation.
public struct MethodImplDecl: Decl, SourceRepresentable {

  public struct Introducer: SourceRepresentable {

    public enum Kind {

      case `let`

      case sink

      case `inout`

    }

    public var range: SourceRange?

    public var kind: Kind

  }

  public enum Body {

    /// An expression body.
    case expr(Expr)

    /// A block body.
    case block(BraceStmt)

  }

  public var range: SourceRange?

  /// The introducer of the method.
  public var introducer: Introducer

  /// The body of the method, if any.
  public var body: Body?

}
