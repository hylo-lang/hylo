/// A map literal expression.
public struct MapLiteralExpr: Expr {

  /// A key-value pair in a map literal.
  public struct Element: Codable {

    var key: AnyExprID

    var value: AnyExprID

  }

  public let origin: SourceRange?

  /// The key-value pairs of the literal.
  public let elements: [Element]

  public init(elements: [Element], origin: SourceRange?) {
    self.origin = origin
    self.elements = elements
  }

}
