/// A boolean literal expression.
public struct BoolLiteralExpr: Expr {

  public static let kind = NodeKind.boolLiteralExpr

  /// The value of the literal.
  public var value: Bool

}
