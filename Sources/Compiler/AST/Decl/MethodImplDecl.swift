/// The declaration of a method implementation.
public struct MethodImplDecl: Decl, LexicalScope {

  public static let kind = NodeKind.methodImplDecl

  public enum Body: Codable {

    /// An expression body.
    case expr(AnyExprID)

    /// A block body.
    case block(NodeID<BraceStmt>)

  }

  /// The introducer of the method.
  public let introducer: SourceRepresentable<ImplIntroducer>

  /// The declaration of the implicit receiver parameter, if any.
  ///
  /// This property is set during type checking.
  public private(set) var receiver: NodeID<ParameterDecl>?

  /// The body of the method, if any.
  public let body: Body?

  /// Creates an instance with the given properties and no `receiver`.
  public init(
    introducer: SourceRepresentable<ImplIntroducer>,
    body: Body? = nil
  ) {
    self.introducer = introducer
    self.body = body
  }

  /// Incorporates the given receiver into `self`.
  ///
  /// - Requires: `self.receiver == nil`
  internal mutating func incorporate(_ receiver: NodeID<ParameterDecl>) {
    precondition(self.receiver == nil)
    self.receiver = receiver
  }
}
