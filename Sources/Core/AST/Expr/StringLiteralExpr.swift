/// A string literal expression.
public struct StringLiteralExpr: Expr {

  public let site: SourceRange

  /// The value of the literal.
  public let value: String

  public init(value: String, site: SourceRange) {
    self.site = site
    self.value = value
  }

}
