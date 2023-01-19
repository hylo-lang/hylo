/// A boolean literal expression.
public struct BooleanLiteralExpr: Expr {

  public let origin: SourceRange

  /// The value of the literal.
  public let value: Bool

  public init(value: Bool, origin: SourceRange) {
    self.origin = origin
    self.value = value
  }

}
