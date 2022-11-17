/// A boolean literal expression.
public struct BooleanLiteralExpr: Expr {

  /// The value of the literal.
  public let value: Bool

  public init(value: Bool) {
    self.value = value
  }

}
