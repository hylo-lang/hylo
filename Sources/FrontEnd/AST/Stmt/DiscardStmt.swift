/// A discard statement.
public struct DiscardStmt: Stmt, Sendable {

  public let site: SourceRange

  /// The expression whose value is discarded.
  public let expr: AnyExprID

  public init(expr: AnyExprID, site: SourceRange) {
    self.site = site
    self.expr = expr
  }

}
