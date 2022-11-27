/// A pattern that matches the value of an equatable expression.
public struct ExprPattern: Pattern {

  public let origin: SourceRange?

  /// The expression of the pattern.
  public let expr: AnyExprID

  public init(expr: AnyExprID, origin: SourceRange?) {
    self.origin = origin
    self.expr = expr
  }

}
