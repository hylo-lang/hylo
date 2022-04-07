/// A buffer literal expression.
public struct BufferLiteralExpr: Expr {

  public var range: SourceRange?

  /// The elements of the literal.
  public var elements: [Expr]

  public func accept<V: ExprVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(bufferLiteral: self)
  }

}
