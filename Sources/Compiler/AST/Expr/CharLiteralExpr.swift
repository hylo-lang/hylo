/// A character literal expression.
public struct CharLiteralExpr: Expr {

  public var range: SourceRange?

  /// The value of the literal.
  public var value: Character

  public func accept<V: ExprVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(charLiteral: self)
  }

}
