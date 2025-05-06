/// A floating-point number literal expression.
public struct FloatLiteralExpr: NumericLiteralExpr, Sendable {

  public let site: SourceRange

  /// The value of the literal.
  public let value: String

  public init(value: String, site: SourceRange) {
    self.site = site
    self.value = value
  }

}
