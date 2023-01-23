/// A union type expression.
public struct UnionTypeExpr: Expr {

  public let site: SourceRange

  /// The elements of the union.
  public let elements: [AnyTypeExprID]

  public init(elements: [AnyTypeExprID], site: SourceRange) {
    self.site = site
    self.elements = elements
  }

}
