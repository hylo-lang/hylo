/// A boolean literal expression.
public struct BoolLiteralExpr: Expr {

  public var range: SourceRange?

  /// The value of the literal.
  public var value: Bool

}
