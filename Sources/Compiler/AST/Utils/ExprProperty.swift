/// A mapping from indices of expressions to properties of a given type.
public struct ExprProperty<Value> {

  /// The internal storage of the map.
  public var storage: [AnyExprID: Value]

  /// Creates an empty node map.
  public init() {
    storage = [:]
  }

  /// Accesses the property associated with the specified ID.
  public subscript<T: ExprID>(id: T) -> Value? {
    _read { yield storage[AnyExprID(id)] }
    _modify { yield &storage[AnyExprID(id)] }
  }

  /// Accesses the property associated with the specified ID.
  public subscript<T: ExprID>(
    id: T,
    default defaultValue: @autoclosure () -> Value
  ) -> Value {
    _read {
      yield storage[AnyExprID(id), default: defaultValue()]
    }
    _modify {
      var value = storage[AnyExprID(id)] ?? defaultValue()
      defer { storage[AnyExprID(id)] = value }
      yield &value
    }
  }

}

extension ExprProperty: Equatable where Value: Equatable {}

extension ExprProperty: Hashable where Value: Hashable {}

extension ExprProperty: Codable where Value: Codable {}
