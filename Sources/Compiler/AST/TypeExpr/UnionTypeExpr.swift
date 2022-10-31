/// A union type expression.
public struct UnionTypeExpr: TypeExpr {

  public static let kind = NodeKind.unionTypeExpr

  /// The elements of the union.
  public let elements: [AnyTypeExprID]

  public init(elements: [AnyTypeExprID] = []) {
    self.elements = elements
  }

}
