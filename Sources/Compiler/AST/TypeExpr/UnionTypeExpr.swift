/// A union type expression.
public struct UnionTypeExpr: Hashable {

  /// The elements of the union.
  public var elements: [SourceRepresentable<TypeExpr>]

}
