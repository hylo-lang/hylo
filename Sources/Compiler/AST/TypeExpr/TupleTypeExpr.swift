/// A tuple type expression.
public struct TupleTypeExpr: TypeExpr {

  public static let kind = NodeKind.tupleTypeExpr

  /// An element in a tuple type expression.
  public struct Element: Hashable {

    /// The label of the element.
    public var label: String?

    /// The type expression of the element.
    public var type: AnyTypeExprID

    public init(label: String? = nil, type: AnyTypeExprID) {
      self.label = label
      self.type = type
    }

  }

  /// The elements of the tuple.
  public var elements: [SourceRepresentable<Element>]

  public init(elements: [SourceRepresentable<TupleTypeExpr.Element>] = []) {
    self.elements = elements
  }

}
