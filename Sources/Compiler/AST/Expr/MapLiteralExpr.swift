/// A map literal expression.
public struct MapLiteralExpr: Expr {

  /// A key-value pair in a map literal.
  public struct Element: Codable {

    var key: AnyExprID

    var value: AnyExprID

  }

  /// The key-value pairs of the literal.
  public let elements: [Element]

  public init(elements: [Element]) {
    self.elements = elements
  }

}
