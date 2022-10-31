/// A case in a match expression.
public struct MatchCase: Node, LexicalScope {

  public static let kind = NodeKind.matchCase

  public enum Body: Codable {

    /// An expression body.
    case expr(AnyExprID)

    /// A block body.
    case block(NodeID<BraceStmt>)

  }

  /// The pattern of the case.
  public let pattern: AnyPatternID

  /// The condition of the case, if any.
  public let condition: AnyExprID?

  /// The body of the case.
  public let body: Body

  public init(pattern: AnyPatternID, condition: AnyExprID? = nil, body: MatchCase.Body) {
    self.pattern = pattern
    self.condition = condition
    self.body = body
  }

}
