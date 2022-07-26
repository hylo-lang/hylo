/// A mapping from indices of declarations to properties of a given type.
public struct DeclMap<Value> {

  /// The internal storage of the map.
  public var storage: [AnyDeclID: Value]

  /// Creates an empty node map.
  public init() {
    storage = [:]
  }

  /// Accesses the property associated with the specified ID.
  public subscript<T: DeclID>(id: T) -> Value? {
    _read { yield storage[AnyDeclID(id)] }
    _modify { yield &storage[AnyDeclID(id)] }
  }

  /// Accesses the property associated with the specified ID.
  public subscript<T: DeclID>(
    id: T,
    default defaultValue: @autoclosure () -> Value
  ) -> Value {
    _read {
      yield storage[AnyDeclID(id), default: defaultValue()]
    }
    _modify {
      var value = storage[AnyDeclID(id)] ?? defaultValue()
      defer { storage[AnyDeclID(id)] = value }
      yield &value
    }
  }

}

extension DeclMap: Equatable where Value: Equatable {}

extension DeclMap: Hashable where Value: Hashable {}

extension DeclMap: Codable where Value: Codable {}
