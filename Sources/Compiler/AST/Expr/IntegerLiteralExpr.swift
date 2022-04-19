/// An integer literal expression.
public struct IntegerLiteralExpr: Expr {

  public static let kind = NodeKind.integerLiteralExpr

  /// The value of the literal.
  public var value: String

}
