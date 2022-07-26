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
  public var pattern: AnyPatternID

  /// The condition of the case, if any.
  public var condition: AnyExprID?

  /// The body of the case.
  public var body: Body

  public init(pattern: AnyPatternID, condition: AnyExprID? = nil, body: MatchCase.Body) {
    self.pattern = pattern
    self.condition = condition
    self.body = body
  }

}
