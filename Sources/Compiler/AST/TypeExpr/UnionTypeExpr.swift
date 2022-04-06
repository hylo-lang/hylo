/// A union type expression.
public struct UnionTypeExpr: TypeExpr {

  public var range: SourceRange?

  /// The elements of the union.
  public var elements: [TypeExpr]

}
