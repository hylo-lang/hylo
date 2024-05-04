/// A tuple expression.
public struct TupleExpr: Expr {

  /// An element in a tuple expression.
  public struct Element: Codable {

    /// The label of the element.
    public var label: SourceRepresentable<Identifier>?

    /// The value of the element.
    public var value: AnyExprID

    public init(label: SourceRepresentable<Identifier>? = nil, value: AnyExprID) {
      self.label = label
      self.value = value
    }

  }

  public let site: SourceRange

  /// The elements of the tuple.
  public let elements: [Element]

  public init(elements: [Element], site: SourceRange) {
    self.site = site
    self.elements = elements
  }

}
