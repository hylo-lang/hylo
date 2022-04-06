/// A string literal expression.
public struct StringLiteralExpr: Expr {

  public var range: SourceRange?

  /// The value of the literal.
  public var value: String

}
