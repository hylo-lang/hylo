/// A map literal expression.
public struct MapLiteralExpr: Expr {

  public var range: SourceRange?

  /// The key-value pairs of the literal.
  public var elements: [(key: Expr, value: Expr)]

  public func accept<V: ExprVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(mapLiteral: self)
  }

}
