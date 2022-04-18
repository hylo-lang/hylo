/// A mapping from indices of expressions to properties of a given type.
public struct ExprMap<Value> {

  /// The internal storage of the map.
  private var storage: [Int: Value]

  /// Creates an empty node map.
  public init() {
    storage = [:]
  }

  /// Accesses the property associated with the specified ID.
  subscript<T: ExprID>(id: T) -> Value? {
    _read   { yield storage[id.rawValue] }
    _modify { yield &storage[id.rawValue] }
  }

  /// Accesses the property associated with the specified ID.
  public subscript<T: ExprID>(
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

extension ExprMap: Equatable where Value: Equatable {}
