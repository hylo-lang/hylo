/// A numeric literal expression.
public protocol NumericLiteralExpr: Expr {

  /// The value of the literal.
  var value: String { get }

}
