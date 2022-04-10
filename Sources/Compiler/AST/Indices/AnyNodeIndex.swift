/// A type-erased index of a node.
public struct AnyNodeIndex: NodeIndexProtocol {

  public let rawValue: NodeIndex.RawValue

  /// The identifier of type of the referred node.
  public let typeID: ObjectIdentifier

  /// Creates a type-erased index from a typed index.
  public init<T: Node>(_ other: NodeIndex<T>) {
    rawValue = other.rawValue
    typeID = ObjectIdentifier(T.self)
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
