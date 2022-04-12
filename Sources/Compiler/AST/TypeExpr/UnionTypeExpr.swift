/// A union type expression.
public struct UnionTypeExpr: TypeExpr {

  public static let kind = NodeKind.unionTypeExpr

  /// The elements of the union.
  public var elements: [AnyTypeExprID]

}
