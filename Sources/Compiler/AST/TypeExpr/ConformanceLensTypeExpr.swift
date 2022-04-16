/// A conformance lens.
public struct ConformanceLensTypeExpr: TypeExpr {

  public static let kind = NodeKind.conformanceLensTypeExpr

  /// The expression of the wrapped type.
  public var wrapped: AnyTypeExprID

  /// The expression of the trait in which the lens focuses.
  public var focus: AnyTypeExprID

}
