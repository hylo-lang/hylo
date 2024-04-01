/// A mapping from expression to `Property` value.
public typealias ExprProperty<Value> = [AnyExprID: Value]

extension Dictionary where Key == AnyExprID {

  /// Accesses the `Property` this map associates with `n`.
  public subscript<T: ExprID>(n: T) -> Value? {
    _read { yield self[AnyExprID(n)] }
    _modify { yield &self[AnyExprID(n)] }
  }

  /// Accesses the `Property` this map associates with `n`, or `defaultProperty` if this map makes
  /// no such association.
  public subscript<T: ExprID>(n: T, default defaultProperty: @autoclosure () -> Value) -> Value {
    _read { yield self[AnyExprID(n), default: defaultProperty()] }
    _modify { yield &self[AnyExprID(n), default: defaultProperty()] }
  }

}
