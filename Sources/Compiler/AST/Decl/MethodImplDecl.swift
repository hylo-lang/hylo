/// The declaration of a method implementation.
public struct MethodImplDecl: Decl {

  public enum Introducer {

    case `let`

    case sink

    case `inout`

  }

  public enum Body {

    /// An expression body.
    case expr(Expr)

    /// A block body.
    case block(BraceStmt)

  }

  /// The introducer of the method.
  public var introducer: SourceRepresentable<Introducer>

  /// The body of the method, if any.
  public var body: SourceRepresentable<Body>?

  public var range: SourceRange?

  public func accept<V: DeclVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(methodImpl: self)
  }

}
