/// An expression statement.
public struct ExprStmt: Stmt {

  public let site: SourceRange

  /// The expression.
  public let expr: AnyExprID

  public init(expr: AnyExprID, site: SourceRange) {
    self.site = site
    self.expr = expr
  }

}
