/// A pattern that matches the value of an equatable expression.
public struct ExprPattern: Pattern {

  public static let kind = NodeKind.exprPattern

  /// The expression of the pattern.
  public var expr: AnyExprIndex

}
