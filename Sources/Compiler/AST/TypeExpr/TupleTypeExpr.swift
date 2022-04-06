/// A tuple type expression.
public struct TupleTypeExpr: TypeExpr {

  /// An element in a tuple type expression.
  public struct Element: SourceRepresentable {

    public var range: SourceRange?

    /// The label of the element.
    public var label: String?

    /// The type expression of the element.
    public var type: TypeExpr

  }

  public var range: SourceRange?

  /// The elements of the tuple.
  public var elements: [Element]

}
