/// The declaration of a method implementation.
public struct MethodImplDecl: Decl, LexicalScope {

  public enum Body: Codable {

    /// An expression body.
    case expr(AnyExprID)

    /// A block body.
    case block(NodeID<BraceStmt>)

  }

  public let origin: SourceRange

  /// The introducer of the method.
  public let introducer: SourceRepresentable<ImplIntroducer>

  /// The declaration of the implicit receiver parameter.
  public let receiver: NodeID<ParameterDecl>

  /// The body of the method, if any.
  public let body: Body?

  /// Creates an instance with the given properties and no `receiver`.
  public init(
    introducer: SourceRepresentable<ImplIntroducer>,
    receiver: NodeID<ParameterDecl>,
    body: Body? = nil,
    origin: SourceRange
  ) {
    self.origin = origin
    self.introducer = introducer
    self.receiver = receiver
    self.body = body
  }

}
