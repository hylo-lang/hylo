/// The declaration of a method implementation.
public struct MethodImplDecl: Decl {

  public static let kind = NodeKind.methodImplDecl

  public enum Introducer: Hashable {

    case `let`

    case sink

    case `inout`

    case assign

  }

  public enum Body: Hashable {

    /// An expression body.
    case expr(AnyExprID)

    /// A block body.
    case block(NodeID<BraceStmt>)

  }

  /// The introducer of the method.
  public var introducer: SourceRepresentable<Introducer>

  /// The body of the method, if any.
  public var body: SourceRepresentable<Body>?

  public init(
    introducer: SourceRepresentable<Introducer>,
    body: SourceRepresentable<Body>? = nil
  ) {
    self.introducer = introducer
    self.body = body
  }

}
