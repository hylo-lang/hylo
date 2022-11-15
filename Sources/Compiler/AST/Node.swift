/// A protocol describing the API of an AST node.
public protocol Node: Codable {

}

extension Node {
  /// A unique identifier denoting the type of this node.
  static var kind: NodeKind { NodeKind(Self.self) }
}
