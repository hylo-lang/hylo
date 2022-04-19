/// An indirect type expression.
public struct IndirectTypeExpr: TypeExpr {

  public static let kind = NodeKind.indirectTypeExpr

  /// The operand.
  public var operand: AnyTypeExprID

  public init(operand: AnyTypeExprID) {
    self.operand = operand
  }

}
