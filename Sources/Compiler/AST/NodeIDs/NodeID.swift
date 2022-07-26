/// A type denoting the ID of a node.
public protocol NodeIDProtocol: Hashable, Codable {

  /// The raw value of the ID.
  var rawValue: NodeID.RawValue { get }

  /// The identifier of type of the referred node.
  var kind: NodeKind { get }

}

/// The ID of a node in an AST.
public struct NodeID<T: Node>: NodeIDProtocol {

  /// The type of a node ID's raw value.
  public typealias RawValue = Int

  public let rawValue: RawValue

  public var kind: NodeKind { T.kind }

  /// Converts `n` to a node ID of type `T`; fails if `n` has a different type.
  public init?<Other: NodeIDProtocol>(converting other: Other) {
    if other.kind == T.kind {
      self.init(unsafeRawValue: other.rawValue)
    } else {
      return nil
    }
  }

  /// Creates a node ID from a raw value, unsafely assuming it has type `T`.
  internal init(unsafeRawValue: RawValue) {
    self.rawValue = unsafeRawValue
  }

  public static func == <Other: NodeIDProtocol>(l: Self, r: Other) -> Bool {
    l.rawValue == r.rawValue
  }

  public static func == <Other: NodeIDProtocol>(l: Other, r: Self) -> Bool {
    l.rawValue == r.rawValue
  }

  public static func != <Other: NodeIDProtocol>(l: Self, r: Other) -> Bool {
    l.rawValue != r.rawValue
  }

  public static func != <Other: NodeIDProtocol>(l: Other, r: Self) -> Bool {
    l.rawValue != r.rawValue
  }

}
