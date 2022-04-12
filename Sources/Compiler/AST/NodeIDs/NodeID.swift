/// A type denoting the ID of a node.
public protocol NodeIDProtocol: Hashable {

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

  internal init(rawValue: RawValue) {
    self.rawValue = rawValue
  }

  public static func == <T: NodeIDProtocol>(l: Self, r: T) -> Bool {
    l.rawValue == r.rawValue
  }

  public static func == <T: NodeIDProtocol>(l: T, r: Self) -> Bool {
    l.rawValue == r.rawValue
  }

  public static func != <T: NodeIDProtocol>(l: Self, r: T) -> Bool {
    l.rawValue != r.rawValue
  }

  public static func != <T: NodeIDProtocol>(l: T, r: Self) -> Bool {
    l.rawValue != r.rawValue
  }

}

/// The type-erased ID of a node.
public struct AnyNodeID: NodeIDProtocol {

  public let rawValue: NodeID.RawValue

  public let kind: NodeKind

  /// Creates a type-erased ID from a node ID.
  public init<T: NodeIDProtocol>(_ other: T) {
    rawValue = other.rawValue
    kind = other.kind
  }

  /// Returns a typed copy of this ID, or `nil` if the type conversion failed.
  public func convert<T: Node>(to: T.Type) -> NodeID<T>? {
    kind == T.kind
      ? NodeID(rawValue: rawValue)
      : nil
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
