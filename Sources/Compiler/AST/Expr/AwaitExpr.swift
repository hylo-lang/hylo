/// An await expression.
public struct AwaitExpr: Expr {

  public static let kind = NodeKind.awaitExpr

  /// The expression of the awaited value.
  public var operand: AnyExprID

}
