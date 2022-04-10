/// A mapping from indices of AST nodes to properties of a given type.
public struct NodeMap<Value> {

  /// The internal storage of the map.
  private var storage: [Int: Value]

  /// Creates an empty node map.
  public init() {
    storage = [:]
  }

  /// Accesses the property associated with the specified index.
  public subscript<T: NodeIndexProtocol>(index: T) -> Value? {
    _read   { yield storage[index.rawValue] }
    _modify { yield &storage[index.rawValue] }
  }

  /// Accesses the property associated with the specified index.
  subscript(raw index: NodeIndex.RawValue) -> Value? {
    _read   { yield storage[index] }
    _modify { yield &storage[index] }
  }

}

extension NodeMap: Equatable where Value: Equatable {}

extension NodeMap: Hashable where Value: Hashable {}
