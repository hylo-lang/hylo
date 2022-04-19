/// A mapping from indices of AST nodes to properties of a given type.
public struct NodeMap<Value> {

  /// The internal storage of the map.
  public var storage: [AnyNodeID: Value]

  /// Creates an empty node map.
  public init() {
    storage = [:]
  }

  /// Accesses the property associated with the specified ID.
  public subscript<T: NodeIDProtocol>(id: T) -> Value? {
    _read { yield storage[AnyNodeID(id)] }
    _modify { yield &storage[AnyNodeID(id)] }
  }

  /// Accesses the property associated with the specified ID.
  public subscript<T: NodeIDProtocol>(
    id: T,
    default defaultValue: @autoclosure () -> Value
  ) -> Value {
    _read {
      yield storage[AnyNodeID(id), default: defaultValue()]
    }
    _modify {
      var value = storage[AnyNodeID(id)] ?? defaultValue()
      defer { storage[AnyNodeID(id)] = value }
      yield &value
    }
  }

}

extension NodeMap: Equatable where Value: Equatable {}

extension NodeMap: Hashable where Value: Hashable {}
