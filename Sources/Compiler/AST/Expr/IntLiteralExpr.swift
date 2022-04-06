/// An integer literal expression.
public struct IntLiteralExpr: Expr {

  public var range: SourceRange?

  /// The value of the literal.
  public var value: String

}
