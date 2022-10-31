/// The declaration of a subscript implementation.
public struct SubscriptImplDecl: Decl, LexicalScope {

  public static let kind = NodeKind.subscriptImplDecl

  public enum Body: Codable {

    /// An expression body.
    case expr(AnyExprID)

    /// A block body.
    case block(NodeID<BraceStmt>)

  }

  /// The introducer of the method.
  public let introducer: SourceRepresentable<ImplIntroducer>

  /// The body of the subscript, if any.
  public let body: Body?

  /// The declaration of the implicit receiver parameter, if any.
  ///
  /// This property is set during type checking.
  public var receiver: NodeID<ParameterDecl>?

  public init(introducer: SourceRepresentable<ImplIntroducer>, body: Body? = nil) {
    self.introducer = introducer
    self.body = body
  }

}
