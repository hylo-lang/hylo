/// A tuple pattern.
public struct TuplePattern: Pattern {

  /// An element in a tuple pattern.
  public struct Element: Codable {

    /// The label of the element.
    public var label: SourceRepresentable<String>?

    /// The pattern of the element.
    public var pattern: AnyPatternID

    public init(label: SourceRepresentable<String>?, pattern: AnyPatternID) {
      self.label = label
      self.pattern = pattern
    }

  }

  public let origin: SourceRange

  /// The elements of the tuple.
  public let elements: [Element]

  public init(elements: [Element], origin: SourceRange) {
    self.origin = origin
    self.elements = elements
  }

}
