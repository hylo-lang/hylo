/// A type denoting the index of a node.
public protocol NodeIndexProtocol: Hashable {

  /// The raw value of the index.
  var rawValue: NodeIndex.RawValue { get }

  /// The identifier of type of the referred node.
  var kind: NodeKind { get }

}

/// The index of a node in an AST.
public struct NodeIndex<T: Node>: NodeIndexProtocol {

  /// The raw type of a node index.
  public typealias RawValue = Int

  public let rawValue: RawValue

  public var kind: NodeKind { T.kind }

  internal init(rawValue: RawValue) {
    self.rawValue = rawValue
  }

  public static func == <T: NodeIndexProtocol>(l: Self, r: T) -> Bool {
    l.rawValue == r.rawValue
  }

  public static func == <T: NodeIndexProtocol>(l: T, r: Self) -> Bool {
    l.rawValue == r.rawValue
  }

  public static func != <T: NodeIndexProtocol>(l: Self, r: T) -> Bool {
    l.rawValue != r.rawValue
  }

  public static func != <T: NodeIndexProtocol>(l: T, r: Self) -> Bool {
    l.rawValue != r.rawValue
  }

}

/// A type-erased index of a node.
public struct AnyNodeIndex: NodeIndexProtocol {

  public let rawValue: NodeIndex.RawValue

  public let kind: NodeKind

  /// Creates a type-erased index from a node index.
  public init<T: NodeIndexProtocol>(_ other: T) {
    rawValue = other.rawValue
    kind = other.kind
  }

  /// Returns a typed copy of this index, or `nil` if the type conversion failed.
  public func convert<T: Node>(to: T.Type) -> NodeIndex<T>? {
    kind == T.kind
      ? NodeIndex(rawValue: rawValue)
      : nil
  }

  /// Indicates whether this index denotes a declaration.

}

extension AnyNodeIndex: Hashable {

  public func hash(into hasher: inout Hasher) {
    rawValue.hash(into: &hasher)
  }

  public static func == (l: Self, r: Self) -> Bool {
    l.rawValue == r.rawValue
  }

}
