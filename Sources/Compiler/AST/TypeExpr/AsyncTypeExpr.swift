/// An asynchronous type expression.
public struct AsyncTypeExpr: TypeExpr {

  public var range: SourceRange?

  /// The operand.
  public var operand: TypeExpr

  public func accept<V: TypeExprVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(async: self)
  }

}
