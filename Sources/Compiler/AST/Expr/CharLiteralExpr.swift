/// A character literal expression.
public struct CharLiteralExpr: Expr {

  public static let kind = NodeKind.charLiteralExpr

  /// The value of the literal.
  public var value: Character

}
