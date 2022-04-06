/// An expression statement.
public struct ExprStmt: Stmt {

  public var range: SourceRange?

  /// The expression.
  public var expr: Expr

}
