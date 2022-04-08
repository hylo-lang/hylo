/// An AST node that may have a textual representation in source code.
///
/// - Note: The source range is a non-salient annotation. It does not contribute to a node's value.
public struct SourceRepresentable<Structure> {

  /// The node.
  public var node: Structure

  /// The source range of the node's textual representation.
  public var range: SourceRange?

  /// Creates a source representable container, annotating a node an an optional source range.
  public init(node: Structure, range: SourceRange? = nil) {
    self.node = node
    self.range = range
  }

}

extension SourceRepresentable: Equatable where Structure: Hashable {

  public static func == (l: Self, r: Self) -> Bool {
    l.node == r.node
  }

}

extension SourceRepresentable: Hashable where Structure: Hashable {

  public func hash(into hasher: inout Hasher) {
    node.hash(into: &hasher)
  }

}
