/// A union type expression.
public struct UnionTypeExpr: Expr {

  public let origin: SourceRange?

  /// The elements of the union.
  public let elements: [AnyTypeExprID]

  public init(elements: [AnyTypeExprID], origin: SourceRange?) {
    self.origin = origin
    self.elements = elements
  }

}
