/// A case in a match expression.
public struct MatchCase: Node, LexicalScope {

  public enum Body: Codable {

    /// An expression body.
    case expr(AnyExprID)

    /// A block body.
    case block(NodeID<BraceStmt>)

  }

  public let origin: SourceRange?

  /// The pattern of the case.
  public let pattern: AnyPatternID

  /// The condition of the case, if any.
  public let condition: AnyExprID?

  /// The body of the case.
  public let body: Body

  public init(
    pattern: AnyPatternID,
    condition: AnyExprID?,
    body: MatchCase.Body,
    origin: SourceRange?
  ) {
    self.origin = origin
    self.pattern = pattern
    self.condition = condition
    self.body = body
  }

}
