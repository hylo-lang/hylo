/// An await expression.
public struct AwaitExpr: Expr {

  public static let kind = NodeKind.awaitExpr

  /// The expression of the awaited value.
  public let operand: AnyExprID

  public init(operand: AnyExprID) {
    self.operand = operand
  }

}
