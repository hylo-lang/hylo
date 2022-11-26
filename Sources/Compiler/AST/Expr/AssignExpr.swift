/// An assignment expression.
public struct AssignExpr: Expr {

  public let origin: SourceRange?

  /// The left operand.
  public let left: AnyExprID

  /// The right operand.
  public let right: AnyExprID

  public init(left: AnyExprID, right: AnyExprID, origin: SourceRange?) {
    self.origin = origin
    self.left = left
    self.right = right
  }

}
