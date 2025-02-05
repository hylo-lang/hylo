/// An integer literal expression.
public struct IntegerLiteralExpr: NumericLiteralExpr, Sendable {

  public let site: SourceRange

  /// The value of the literal.
  public let value: String

  public init(value: String, site: SourceRange) {
    self.site = site
    self.value = value
  }

}
