/// An integer literal expression.
public struct IntegerLiteralExpr: Expr {

  public let origin: SourceRange?

  /// The value of the literal.
  public let value: String

  public init(value: String, origin: SourceRange?) {
    self.origin = origin
    self.value = value
  }

}
