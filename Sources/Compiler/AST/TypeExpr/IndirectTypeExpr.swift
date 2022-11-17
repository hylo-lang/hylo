/// An indirect type expression.
public struct IndirectTypeExpr: TypeExpr {

  /// The operand.
  public let operand: AnyTypeExprID

  public init(operand: AnyTypeExprID) {
    self.operand = operand
  }

}
