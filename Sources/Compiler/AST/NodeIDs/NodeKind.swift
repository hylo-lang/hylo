import Utils

/// The type of an AST node; a nominal wrapper for Node.Type that adds conformances and convenience
/// APIs.
public struct NodeKind: Codable, Equatable, Hashable {

  /// The underlying value.
  let value: Node.Type

  /// Creates an instance with the given underlying value.
  public init(_ value: Node.Type) {
    self.value = value
  }

  /// Serializes `self` into `destination`.
  public func encode(to destination: Encoder) throws {
    try Utils.encode(value, to: destination)
  }

  /// Deserializes `self` from `source`.
  public init(from source: Decoder) throws {
    let t = try decodeMetatype(from: source)
    guard let nodeType = t as? Node.Type else {
      throw DecodingError.typeMismatch(
        NodeKind.self,
        .init(
          codingPath: source.codingPath,
          debugDescription: "\(t) is not a Node type.",
          underlyingError: nil))
      
    }
    self.value = nodeType
  }

  /// Returns true iff `l` and `r` denote the same node type.
  public static func == (l: Self, r: Self) -> Bool {
    return l.value == r.value
  }

  /// Incorporates the value of `self` into `h`.
  public func hash(into h: inout Hasher) {
    ObjectIdentifier(value).hash(into: &h)
  }

  /// Returns true iff `l` and `r` denote the same node type.
  static func == (l: Self, r: Node.Type) -> Bool {
    return l.value == r
  }
  
  /// Returns true iff `l` and `r` do not denote the same node type.
  static func != (l: Self, r: Node.Type) -> Bool {
    return l.value == r
  }

  /// Returns true iff `l` and `r` denote the same node type.
  static func == (l: Node.Type, r: Self) -> Bool {
    return l == r.value
  }
  
  /// Returns true iff `l` and `r` do not denote the same node type.
  static func != (l: Node.Type, r: Self) -> Bool {
    return l != r.value
  }

  /// Returns true iff `me` and `pattern` denote the same node type.
  static func ~=(pattern: Node.Type, me: Self) -> Bool {
    me == pattern
  }

}

/// Extend heterogeneous equality comparison with Node.Type to Optional<NodeKind>.
extension Optional where Wrapped == NodeKind {
  /// Returns true iff `l` and `r` denote the same node type.
  static func == (l: Self, r: Node.Type) -> Bool {
    return l?.value == r
  }

  /// Returns true iff `l` and `r` do not denote the same node type.
  static func != (l: Self, r: Node.Type) -> Bool {
    return l?.value == r
  }

  /// Returns true iff `l` and `r` denote the same node type.
  static func == (l: Node.Type, r: Self) -> Bool {
    return l == r?.value
  }

  /// Returns true iff `l` and `r` do not denote the same node type.
  static func != (l: Node.Type, r: Self) -> Bool {
    return l != r?.value
  }

}

extension NodeKind: CustomStringConvertible {

  /// The name of the underlying Node type.
  public var description: String { String(describing: value) }

}
