/// A conformance lens.
public struct ConformanceLensTypeExpr: TypeExpr {

  /// The expression of the lens' base.
  public var base: AnyTypeExprIndex

  /// The name of the trait in which the lens focuses.
  public var trait: SourceRepresentable<Identifier>

}
