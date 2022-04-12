/// A mapping from indices of AST nodes to properties of a given type.
public struct NodeMap<Value> {

  /// The internal storage of the map.
  private var storage: [Int: Value]

  /// Creates an empty node map.
  public init() {
    storage = [:]
  }

  /// Accesses the property associated with the specified ID.
  public subscript<T: NodeIDProtocol>(id: T) -> Value? {
    _read   { yield storage[id.rawValue] }
    _modify { yield &storage[id.rawValue] }
  }

  /// Accesses the property associated with the specified ID.
  public subscript<T: NodeIDProtocol>(
    id: T,
    default defaultValue: @autoclosure () -> Value
  ) -> Value {
    _read   { yield storage[id.rawValue, default: defaultValue()] }
    _modify {
      var value = storage[id.rawValue] ?? defaultValue()
      defer { storage[id.rawValue] = value }
      yield &value
    }
  }

  /// Accesses the property associated with the specified ID.
  subscript(raw id: NodeID.RawValue) -> Value? {
    _read   { yield storage[id] }
    _modify { yield &storage[id] }
  }

}

extension NodeMap: Equatable where Value: Equatable {}

extension NodeMap: Hashable where Value: Hashable {}
