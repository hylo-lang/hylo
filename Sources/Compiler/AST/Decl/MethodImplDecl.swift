/// The declaration of a method implementation.
public struct MethodImplDecl: Decl {

  public enum Introducer: Hashable {

    case `let`

    case sink

    case `inout`

  }

  public enum Body: Hashable {

    /// An expression body.
    case expr(AnyExprIndex)

    /// A block body.
    case block(NodeIndex<BraceStmt>)

  }

  /// The introducer of the method.
  public var introducer: SourceRepresentable<Introducer>

  /// The body of the method, if any.
  public var body: SourceRepresentable<Body>?

}
