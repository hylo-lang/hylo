/// A string literal expression.
public struct StringLiteralExpr: Expr {

  public static let kind = NodeKind.stringLiteralExpr

  /// The value of the literal.
  public let value: String

  public init(value: String) {
    self.value = value
  }

}
