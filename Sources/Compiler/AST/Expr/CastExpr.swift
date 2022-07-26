/// An explicit cast expression.
public struct CastExpr: Expr {

  public static let kind = NodeKind.castExpr

  /// The direction of a cast.
  public enum Direction: Codable {

    case up

    case down

  }

  /// The left operand.
  public var left: AnyExprID

  /// The type to which the left operand is being converted.
  public var right: AnyTypeExprID

  /// The direction of the cast.
  public var direction: Direction

  public init(left: AnyExprID, right: AnyTypeExprID, direction: Direction) {
    self.left = left
    self.right = right
    self.direction = direction
  }

}
