/// A map literal expression.
public struct MapLiteralExpr: Hashable {

  /// A key-value pair in a map literal.
  public struct Element: Hashable {

    var key: SourceRepresentable<Expr>

    var value: SourceRepresentable<Expr>

  }

  /// The key-value pairs of the literal.
  public var elements: [SourceRepresentable<Element>]

}
