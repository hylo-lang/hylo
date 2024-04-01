/// A match expression.
public struct MatchExpr: Expr {

  public let site: SourceRange

  /// The site of the `match` keyword.
  public let introducerSite: SourceRange

  /// The subject of the match.
  public let subject: AnyExprID

  /// The cases of the match.
  public let cases: [MatchCase.ID]

  public init(
    introducerSite: SourceRange,
    subject: AnyExprID,
    cases: [MatchCase.ID],
    site: SourceRange
  ) {
    self.site = site
    self.introducerSite = introducerSite
    self.subject = subject
    self.cases = cases
  }

}
