/// A boolean literal expression.
public struct BooleanLiteralExpr: Expr {

  public let site: SourceRange

  /// The value of the literal.
  public let value: Bool

  public init(value: Bool, site: SourceRange) {
    self.site = site
    self.value = value
  }

}
