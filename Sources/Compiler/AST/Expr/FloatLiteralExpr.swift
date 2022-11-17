/// A floating-point number literal expression.
public struct FloatLiteralExpr: Expr {

  /// The value of the literal.
  public let value: String

  public init(value: String) {
    self.value = value
  }

}
