/// An await expression.
public struct AwaitExpr: Expr {

  public let origin: SourceRange?

  /// The expression of the awaited value.
  public let operand: AnyExprID

  public init(operand: AnyExprID, origin: SourceRange) {
    self.origin = origin
    self.operand = operand
  }

}
