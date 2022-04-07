/// An indirect type expression.
public struct IndirectTypeExpr: TypeExpr {

  public var range: SourceRange?

  /// The operand.
  public var operand: TypeExpr

  public func accept<V: TypeExprVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(indirect: self)
  }

}
