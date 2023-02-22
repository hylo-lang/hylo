/// The declaration of a subscript implementation.
/// ///
/// Instances of this type represent individual variant inside a subscript declaration.
public struct SubscriptImpl: Decl, LexicalScope {

  /// The body of a subscript implementation.
  public enum Body: Codable {

    /// An expression body.
    case expr(AnyExprID)

    /// A block body.
    case block(BraceStmt.ID)

    /// The node wrapped by this instance.
    public var base: AnyNodeID {
      switch self {
      case .expr(let n):
        return AnyNodeID(n)
      case .block(let n):
        return AnyNodeID(n)
      }
    }

  }

  public let site: SourceRange

  /// The introducer of the subscript.
  public let introducer: SourceRepresentable<AccessEffect>

  /// The declaration of the implicit receiver parameter, if any.
  public let receiver: ParameterDecl.ID?

  /// The body of the subscript, if any.
  public let body: Body?

  public init(
    introducer: SourceRepresentable<AccessEffect>,
    receiver: ParameterDecl.ID?,
    body: Body?,
    site: SourceRange
  ) {
    self.site = site
    self.introducer = introducer
    self.receiver = receiver
    self.body = body
  }

}
