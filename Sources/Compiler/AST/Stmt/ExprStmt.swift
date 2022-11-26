/// An expression statement.
public struct ExprStmt: Stmt {

  public let origin: SourceRange?

  /// The expression.
  public let expr: AnyExprID

  public init(expr: AnyExprID, origin: SourceRange?) {
    self.origin = origin
    self.expr = expr
  }

}
