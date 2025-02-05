/// A numeric literal expression.
public protocol NumericLiteralExpr: Expr, Sendable {

  /// The value of the literal.
  var value: String { get }

}
