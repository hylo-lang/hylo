/// A floating-point number literal expression.
public struct FloatLiteralExpr: Expr {

  public var range: SourceRange?

  /// The value of the literal.
  public var value: String

}
