/// A pattern that matches the value of an equatable expression.
public struct ExprPattern: Pattern {

  /// The expression of the pattern.
  public let expr: AnyExprID

  public init(expr: AnyExprID) {
    self.expr = expr
  }

}
