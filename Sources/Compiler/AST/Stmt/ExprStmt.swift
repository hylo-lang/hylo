/// An expression statement.
public struct ExprStmt: Stmt {

  public static let kind = NodeKind.exprStmt

  /// The expression.
  public var expr: AnyExprID

  public init(expr: AnyExprID) {
    self.expr = expr
  }

}
