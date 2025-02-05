/// A tuple type expression.
public struct TupleTypeExpr: Expr, Sendable {

  /// An element in a tuple type expression.
  public struct Element: Codable, Sendable {

    /// The label of the element.
    public var label: SourceRepresentable<Identifier>?

    /// The type expression of the element.
    public var type: AnyExprID

    public init(label: SourceRepresentable<Identifier>? = nil, type: AnyExprID) {
      self.label = label
      self.type = type
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
