/// A mapping from indices of declarations to properties of a given type.
public struct DeclMap<Value> {

  /// The internal storage of the map.
  private var storage: [Int: Value]

  /// Creates an empty node map.
  public init() {
    storage = [:]
  }

  /// Accesses the property associated with the specified index.
  subscript<T: DeclIndex>(index: T) -> Value? {
    _read   { yield storage[index.rawValue] }
    _modify { yield &storage[index.rawValue] }
  }

  /// Accesses the property associated with the specified index.
  public subscript<T: DeclIndex>(
    index: T,
    default defaultValue: @autoclosure () -> Value
  ) -> Value {
    _read   { yield storage[index.rawValue, default: defaultValue()] }
    _modify {
      var value = storage[index.rawValue] ?? defaultValue()
      defer { storage[index.rawValue] = value }
      yield &value
    }
  }

}

extension DeclMap: Equatable where Value: Equatable {}
