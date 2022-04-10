/// A of an AST node that may have a textual representation in source code.
///
/// - Note: The source range is a non-salient annotation. It does not contribute to a node's value.
public struct SourceRepresentable<Part> {

  /// The part.
  public var value: Part

  /// The source range of the node's textual representation.
  public var range: SourceRange?

  /// Creates a source representable container, annotating a value with an optional source range.
  public init(value: Part, range: SourceRange? = nil) {
    self.value = value
    self.range = range
  }

}

extension SourceRepresentable: Equatable where Part: Equatable {

  public static func == (l: Self, r: Self) -> Bool {
    l.value == r.value
  }

}

extension SourceRepresentable: Hashable where Part: Hashable {

  public func hash(into hasher: inout Hasher) {
    value.hash(into: &hasher)
  }

}
