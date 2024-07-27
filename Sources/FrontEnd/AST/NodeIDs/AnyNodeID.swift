/// The type-erased ID of a node.
public struct AnyNodeID: NodeIDProtocol {

  public let rawValue: NodeRawIdentity

  public let kind: NodeKind

  /// Creates a type-erased ID from a node ID.
  public init<T: NodeIDProtocol>(_ other: T) {
    rawValue = other.rawValue
    kind = other.kind
  }

}

extension AnyNodeID: Hashable {

  public func hash(into hasher: inout Hasher) {
    rawValue.hash(into: &hasher)
  }

  public static func == (l: Self, r: Self) -> Bool {
    l.rawValue == r.rawValue
  }

}
