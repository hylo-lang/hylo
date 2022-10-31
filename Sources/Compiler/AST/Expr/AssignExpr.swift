/// An assignment expression.
public struct AssignExpr: Expr {

  public static let kind = NodeKind.assignExpr

  /// The left operand.
  public let left: AnyExprID

  /// The right operand.
  public let right: AnyExprID

  public init(left: AnyExprID, right: AnyExprID) {
    self.left = left
    self.right = right
  }

}
