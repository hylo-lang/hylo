/// An assignment expression.
public struct AssignExpr: Expr {

  public static let kind = NodeKind.assignExpr

  /// The left operand.
  public var left: AnyExprID

  /// The right operand.
  public var right: AnyExprID

  public init(left: AnyExprID, right: AnyExprID) {
    self.left = left
    self.right = right
  }

}
