/// A do-while loop.
public struct DoWhileStmt: Stmt, Sendable {

  public let site: SourceRange

  /// The site of the `do` introducer.
  public let introducerSite: SourceRange

  /// The body of the loop.
  public let body: BraceStmt.ID

  /// The condition of the loop.
  ///
  /// - Note: The condition is evaluated in the lexical scope of the body.
  public let condition: Introduced<AnyExprID>

  public init(
    introducerSite: SourceRange, body: BraceStmt.ID, condition: Introduced<AnyExprID>,
    site: SourceRange
  ) {
    self.site = site
    self.introducerSite = introducerSite
    self.body = body
    self.condition = condition
  }

}
