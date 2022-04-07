/// An integer literal expression.
public struct IntLiteralExpr: Expr {

  public var range: SourceRange?

  /// The value of the literal.
  public var value: String

  public func accept<V: ExprVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(intLiteral: self)
  }

}
