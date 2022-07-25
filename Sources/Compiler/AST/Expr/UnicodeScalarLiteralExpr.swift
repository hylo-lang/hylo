/// A unicode scalar literal expression.
public struct UnicodeScalarLiteralExpr: Expr {

  public static let kind = NodeKind.unicodeScalarLiteralExpr

  /// The value of the literal.
  public var value: Character

  public init(value: Character) {
    self.value = value
  }

}
