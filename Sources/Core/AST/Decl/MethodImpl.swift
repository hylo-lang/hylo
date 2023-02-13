/// The declaration of a method implementation.
///
/// Instances of this type represent individual variant inside a method declaration.
public struct MethodImpl: Decl, LexicalScope {

  public enum Body: Codable {

    /// An expression body.
    case expr(AnyExprID)

    /// A block body.
    case block(NodeID<BraceStmt>)

  }

  public let site: SourceRange

  /// The introducer of the method.
  public let introducer: SourceRepresentable<AccessEffect>

  /// The declaration of the implicit receiver parameter.
  public let receiver: NodeID<ParameterDecl>

  /// The body of the method, if any.
  public let body: Body?

  /// Creates an instance with the given properties and no `receiver`.
  public init(
    introducer: SourceRepresentable<AccessEffect>,
    receiver: NodeID<ParameterDecl>,
    body: Body? = nil,
    site: SourceRange
  ) {
    self.site = site
    self.introducer = introducer
    self.receiver = receiver
    self.body = body
  }

}
