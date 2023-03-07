/// A case in a match expression.
public struct MatchCase: Node, LexicalScope {

  public enum Body: Codable {

    /// An expression body.
    case expr(AnyExprID)

    /// A block body.
    case block(BraceStmt.ID)

  }

  public let site: SourceRange

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
    site: SourceRange
  ) {
    self.site = site
    self.pattern = pattern
    self.condition = condition
    self.body = body
  }

}
