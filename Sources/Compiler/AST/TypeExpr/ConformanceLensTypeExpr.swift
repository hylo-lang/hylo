/// A conformance lens.
public struct ConformanceLensTypeExpr: TypeExpr {

  public static let kind = NodeKind.conformanceLensTypeExpr

  /// The expression of the lens' base.
  public var base: AnyTypeExprID

  /// The expression of the trait in which the lens focuses.
  public var trait: AnyTypeExprID

}
