/// An expression statement.
public struct ExprStmt: Stmt {

  public static let kind = NodeKind(ExprStmt.self)

  /// The expression.
  public let expr: AnyExprID

  public init(expr: AnyExprID) {
    self.expr = expr
  }

}
