/// A floating-point number literal expression.
public struct FloatLiteralExpr: Expr {

  public let origin: SourceRange

  /// The value of the literal.
  public let value: String

  public init(value: String, origin: SourceRange) {
    self.origin = origin
    self.value = value
  }

}
