/// A mapping from AST node to `Property` value.
public struct NodeMap<Property> {

  /// The internal storage of the map.
  private var storage: [AnyNodeID: Property]

  /// Creates an empty instance.
  public init() {
    storage = [:]
  }

  /// Accesses the `Property` this map associates with `id`.
  public subscript<T: NodeIDProtocol>(id: T) -> Property? {
    _read { yield storage[AnyNodeID(id)] }
    _modify { yield &storage[AnyNodeID(id)] }
  }

  /// Accesses the property this map associates with `id`, or `defaultProperty` if this map makes
  /// no such association.
  public subscript<T: NodeIDProtocol>(
    id: T,
    default defaultProperty: @autoclosure () -> Property
  ) -> Property {
    _read {
      yield storage[AnyNodeID(id), default: defaultProperty()]
    }
    _modify {
      var value = storage[AnyNodeID(id)] ?? defaultProperty()
      defer { storage[AnyNodeID(id)] = value }
      yield &value
    }
  }

}

extension NodeMap: Equatable where Property: Equatable {}

extension NodeMap: Hashable where Property: Hashable {}

extension NodeMap: Codable where Property: Codable {}
