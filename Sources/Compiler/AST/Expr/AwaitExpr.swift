/// An await expression.
public struct AwaitExpr: Hashable {

  /// The expression of the awaited value.
  public var operand: SourceRepresentable<Expr>

}
