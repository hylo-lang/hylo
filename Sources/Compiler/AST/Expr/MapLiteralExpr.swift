/// A map literal expression.
public struct MapLiteralExpr: Expr {

  public static let kind = NodeKind.mapLiteralExpr

  /// A key-value pair in a map literal.
  public struct Element: Hashable {

    var key: AnyExprID

    var value: AnyExprID

  }

  /// The key-value pairs of the literal.
  public var elements: [SourceRepresentable<Element>]

  public init(elements: [SourceRepresentable<MapLiteralExpr.Element>]) {
    self.elements = elements
  }

}
