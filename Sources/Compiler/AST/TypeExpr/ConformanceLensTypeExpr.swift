/// A conformance lens.
public struct ConformanceLensTypeExpr: Hashable {

  /// The expression of the lens' base.
  public var base: SourceRepresentable<TypeExpr>

  /// The name of the trait in which the lens focuses.
  public var trait: SourceRepresentable<Identifier>

}
