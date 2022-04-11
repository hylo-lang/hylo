/// An AST node.
public protocol Node {

  /// A unique identifier denoting the type of this node.
  static var kind: NodeKind { get }

}
