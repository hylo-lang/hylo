/// A union type expression.
public struct UnionTypeExpr: TypeExpr {

  public var range: SourceRange?

  /// The elements of the union.
  public var elements: [TypeExpr]

  public func accept<V: TypeExprVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(union: self)
  }

}
