/// A union type expression.
public struct UnionTypeExpr: Expr {

  public let site: SourceRange

  /// The elements of the union.
  public let elements: [AnyExprID]

  public init(elements: [AnyExprID], site: SourceRange) {
    self.site = site
    self.elements = elements
  }

}
