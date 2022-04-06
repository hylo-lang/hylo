/// An AST node that may have a textual representation in source code.
public protocol SourceRepresentable {

  /// The source range of the node's textual representation.
  var range: SourceRange? { get }

}
