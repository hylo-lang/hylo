/// An integer literal expression.
public struct IntegerLiteralExpr: Expr {

  /// The value of the literal.
  public let value: String

  public init(value: String) {
    self.value = value
  }

}
