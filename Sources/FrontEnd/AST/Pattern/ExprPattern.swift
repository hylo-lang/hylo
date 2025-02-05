/// A pattern that matches the value of an equatable expression.
public struct ExprPattern: Pattern, Sendable {

  public let site: SourceRange

  /// The expression of the pattern.
  public let expr: AnyExprID

  public init(expr: AnyExprID, site: SourceRange) {
    self.site = site
    self.expr = expr
  }

}
