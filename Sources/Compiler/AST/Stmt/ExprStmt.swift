/// An expression statement.
public struct ExprStmt: Stmt {

  public static let kind = NodeKind.exprStmt

  /// The expression.
  public var expr: AnyExprIndex

}
