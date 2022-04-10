/// A tuple pattern.
public struct TuplePattern: Pattern {

  /// An element in a tuple pattern.
  public struct Element: Hashable {

    /// The label of the element.
    public var label: String?

    /// The pattern of the element.
    public var pattern: AnyPatternIndex

  }

  /// The elements of the tuple.
  public var elements: [SourceRepresentable<Element>]

}
