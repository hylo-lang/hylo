/// An asynchronous type expression.
public struct AsyncTypeExpr: TypeExpr {

  public static let kind = NodeKind.asyncTypeExpr

  /// The operand.
  public var operand: AnyTypeExprIndex

}
