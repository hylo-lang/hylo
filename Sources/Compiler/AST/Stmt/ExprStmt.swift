/// An expression statement.
public struct ExprStmt: Stmt {

  /// The expression.
  public let expr: AnyExprID

  public init(expr: AnyExprID) {
    self.expr = expr
  }

}
