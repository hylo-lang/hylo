/// The declaration of a subscript implementation.
public struct SubscriptImplDecl: Decl, LexicalScope {

  public enum Body: Codable {

    /// An expression body.
    case expr(AnyExprID)

    /// A block body.
    case block(NodeID<BraceStmt>)

  }

  /// The introducer of the method.
  public let introducer: SourceRepresentable<ImplIntroducer>

  /// The declaration of the implicit receiver parameter, if any.
  public let receiver: NodeID<ParameterDecl>

  /// The body of the subscript, if any.
  public let body: Body?

  public init(
    introducer: SourceRepresentable<ImplIntroducer>,
    receiver: NodeID<ParameterDecl>,
    body: Body?
  ) {
    self.introducer = introducer
    self.receiver = receiver
    self.body = body
  }

}
