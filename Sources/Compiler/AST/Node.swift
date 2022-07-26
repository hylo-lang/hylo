/// A protocol describing the API of an AST node.
public protocol Node: Codable {

  /// A unique identifier denoting the type of this node.
  static var kind: NodeKind { get }

}
