/// A do-while loop.
public struct DoWhileStmt: Stmt {

  public let site: SourceRange

  /// The body of the loop.
  public let body: BraceStmt.ID

  /// The condition of the loop.
  ///
  /// - Note: The condition is evaluated in the lexical scope of the body.
  public let condition: AnyExprID

  public init(body: BraceStmt.ID, condition: AnyExprID, site: SourceRange) {
    self.site = site
    self.body = body
    self.condition = condition
  }

}
