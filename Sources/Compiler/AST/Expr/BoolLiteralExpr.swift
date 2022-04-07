/// A boolean literal expression.
public struct BoolLiteralExpr: Expr {

  public var range: SourceRange?

  /// The value of the literal.
  public var value: Bool

  public func accept<V: ExprVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(boolLiteral: self)
  }

}
