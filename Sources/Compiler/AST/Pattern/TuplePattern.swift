/// A tuple pattern.
public struct TuplePattern: Pattern {

  public static let kind = NodeKind.tuplePattern

  /// An element in a tuple pattern.
  public struct Element: Hashable {

    /// The label of the element.
    public var label: String?

    /// The pattern of the element.
    public var pattern: AnyPatternID

  }

  /// The elements of the tuple.
  public var elements: [SourceRepresentable<Element>]

}
