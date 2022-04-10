/// A union type expression.
public struct UnionTypeExpr: TypeExpr {

  /// The elements of the union.
  public var elements: [AnyTypeExprIndex]

}
