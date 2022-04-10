/// A type denoting the index of a node.
public protocol NodeIndexProtocol: Hashable {

  /// The raw value of the index.
  var rawValue: NodeIndex.RawValue { get }

  /// The identifier of type of the referred node.
  var typeID: ObjectIdentifier { get }

}

/// The index of a node in an AST.
public struct NodeIndex<T: Node>: NodeIndexProtocol {

  /// The raw type of a node index.
  public typealias RawValue = Int

  public let rawValue: RawValue

  public var typeID: ObjectIdentifier { ObjectIdentifier(T.self) }

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

  public let typeID: ObjectIdentifier

  /// Creates a type-erased index from a node index.
  public init<T: NodeIndexProtocol>(_ other: T) {
    rawValue = other.rawValue
    typeID = other.typeID
  }

  /// Returns a typed copy of this index, or `nil` if the type conversion failed.
  public func convert<T: Node>(to: T.Type) -> NodeIndex<T>? {
    typeID == ObjectIdentifier(T.self)
      ? NodeIndex(rawValue: rawValue)
      : nil
  }

}

extension AnyNodeIndex: Hashable {

  public func hash(into hasher: inout Hasher) {
    rawValue.hash(into: &hasher)
  }

  public static func == (l: Self, r: Self) -> Bool {
    l.rawValue == r.rawValue
  }

}
