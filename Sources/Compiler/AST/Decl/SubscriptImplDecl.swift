/// The declaration of a subscript implementation.
public struct SubscriptImplDecl: Decl, LexicalScope {

  public static let kind = NodeKind.subscriptImplDecl

  public enum Introducer: Hashable {

    case `let`

    case `inout`

    case set

    case sink

  }

  public enum Body: Hashable {

    /// An expression body.
    case expr(AnyExprID)

    /// A block body.
    case block(NodeID<BraceStmt>)

  }

  /// The introducer of the method.
  public var introducer: SourceRepresentable<Introducer>

  /// The body of the subscript, if any.
  public var body: Body?

  public init(introducer: SourceRepresentable<Introducer>, body: Body? = nil) {
    self.introducer = introducer
    self.body = body
  }

}
