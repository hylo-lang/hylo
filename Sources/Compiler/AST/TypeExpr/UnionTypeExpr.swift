/// A union type expression.
public struct UnionTypeExpr: TypeExpr {

  /// The elements of the union.
  public let elements: [AnyTypeExprID]

  public init(elements: [AnyTypeExprID] = []) {
    self.elements = elements
  }

}
