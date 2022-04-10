/// A map literal expression.
public struct MapLiteralExpr: Expr {

  /// A key-value pair in a map literal.
  public struct Element: Hashable {

    var key: AnyExprIndex

    var value: AnyExprIndex

  }

  /// The key-value pairs of the literal.
  public var elements: [SourceRepresentable<Element>]

}
