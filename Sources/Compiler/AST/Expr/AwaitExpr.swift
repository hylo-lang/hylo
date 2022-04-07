/// An await expression.
public struct AwaitExpr: Expr {

  public var range: SourceRange?

  /// The expression of the awaited value.
  public var operand: Expr

  public func accept<V: ExprVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(await: self)
  }

}
