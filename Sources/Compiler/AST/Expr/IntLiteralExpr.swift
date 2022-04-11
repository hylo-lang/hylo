/// An integer literal expression.
public struct IntLiteralExpr: Expr {

  public static let kind = NodeKind.intLiteralExpr

  /// The value of the literal.
  public var value: String

}
