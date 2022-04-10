/// A tuple expression.
public struct TupleExpr: Expr {

  /// An element in a tuple expression.
  public struct Element: Hashable {

    /// The label of the element.
    public var label: String?

    /// The value of the element.
    public var value: AnyExprIndex

  }

  /// The elements of the tuple.
  public var elements: [SourceRepresentable<Element>]

}
