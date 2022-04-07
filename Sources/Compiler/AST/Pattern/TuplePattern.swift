/// A tuple pattern.
public struct TuplePattern: Pattern {

  /// An element in a tuple pattern.
  public struct Element: SourceRepresentable {

    public var range: SourceRange?

    /// The label of the element.
    public var label: String?

    /// The pattern of the element.
    public var pattern: Pattern

  }

  public var range: SourceRange?

  /// The elements of the tuple.
  public var elements: [Element]

  public func accept<V: PatternVisitor>(_ visitor: inout V) -> V.Result {
    visitor.visit(tuple: self)
  }

}
