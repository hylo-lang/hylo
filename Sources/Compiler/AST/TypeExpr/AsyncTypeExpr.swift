/// An asynchronous type expression.
public struct AsyncTypeExpr: TypeExpr {

  /// The operand.
  public let operand: AnyTypeExprID

  public init(operand: AnyTypeExprID) {
    self.operand = operand
  }

}
