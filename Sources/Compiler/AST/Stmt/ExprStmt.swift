/// An expression statement.
public struct ExprStmt: Stmt {

  public var range: SourceRange?

  /// The expression.
  public var expr: Expr

  public func accept<V: StmtVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(expr: self)
  }

}
