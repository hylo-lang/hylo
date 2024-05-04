/// A mapping from expression to `Property` value.
public typealias DeclProperty<Value> = [AnyDeclID: Value]

extension Dictionary where Key == AnyDeclID {

  /// Accesses the `Property` this map associates with `n`.
  public subscript<T: DeclID>(n: T) -> Value? {
    _read { yield self[AnyDeclID(n)] }
    _modify { yield &self[AnyDeclID(n)] }
  }

  /// Accesses the `Property` this map associates with `n`, or `defaultProperty` if this map makes
  /// no such association.
  public subscript<T: DeclID>(n: T, default defaultProperty: @autoclosure () -> Value) -> Value {
    _read { yield self[AnyDeclID(n), default: defaultProperty()] }
    _modify { yield &self[AnyDeclID(n), default: defaultProperty()] }
  }

}
