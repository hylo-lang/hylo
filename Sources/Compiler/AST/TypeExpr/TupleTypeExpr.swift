/// A tuple type expression.
public struct TupleTypeExpr: Hashable {

  /// An element in a tuple type expression.
  public struct Element: Hashable {

    /// The label of the element.
    public var label: String?

    /// The type expression of the element.
    public var type: SourceRepresentable<TypeExpr>

  }

  /// The elements of the tuple.
  public var elements: [SourceRepresentable<Element>]

}
