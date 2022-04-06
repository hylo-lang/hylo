/// A character literal expression.
public struct CharLiteralExpr: Expr {

  public var range: SourceRange?

  /// The value of the literal.
  public var value: Character

}
