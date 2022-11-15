import Utils

/// A type-erasing container for AST nodes.
struct AnyNode: Codable {

  fileprivate enum CodingKeys: String, CodingKey {

    case kind, data

  }

  /// The wrapped node.
  let node: Node

  /// Creates a type-erased container that wraps `node`.
  init(_ node: Node) {
    self.node = node
  }

  init(from decoder: Decoder) throws {
    let container = try decoder.container(keyedBy: CodingKeys.self)

    let kind = try container.decode(NodeKind.self, forKey: .kind)
    node = try container.decode(kind.value)
  }

  func encode(to encoder: Encoder) throws {
    var container = encoder.container(keyedBy: CodingKeys.self)

    let kind = type(of: node).kind
    try container.encode(kind, forKey: .kind)
    try container.encode(node)
  }
}

fileprivate extension KeyedDecodingContainer where K == AnyNode.CodingKeys {

  func decode<T: Node>(_ type: T.Type) throws -> T {
    try decode(type, forKey: .data)
  }

}


fileprivate extension KeyedEncodingContainer where K == AnyNode.CodingKeys {

  mutating func encode<T: Node>(_ node: T) throws {
    try encode(node, forKey: .data)
  }

}
