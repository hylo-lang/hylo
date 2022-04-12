/// A mapping from indices of declarations to properties of a given type.
public struct DeclMap<Value> {

  /// The internal storage of the map.
  private var storage: [Int: Value]

  /// Creates an empty node map.
  public init() {
    storage = [:]
  }

  /// Accesses the property associated with the specified ID.
  subscript<T: DeclID>(id: T) -> Value? {
    _read   { yield storage[id.rawValue] }
    _modify { yield &storage[id.rawValue] }
  }

  /// Accesses the property associated with the specified ID.
  public subscript<T: DeclID>(
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

}

extension DeclMap: Equatable where Value: Equatable {}
