/// A mapping from pattern to `Property` value.
public typealias PatternProperty<Value> = [AnyPatternID: Value]

extension Dictionary where Key == AnyPatternID {

  /// Accesses the `Property` this map associates with `n`.
  public subscript<T: PatternID>(p: T) -> Value? {
    _read { yield self[AnyPatternID(p)] }
    _modify { yield &self[AnyPatternID(p)] }
  }

  /// Accesses the `Property` this map associates with `n`, or `defaultProperty` if this map makes
  /// no such association.
  public subscript<T: PatternID>(
    p: T, default defaultProperty: @autoclosure () -> Value
  ) -> Value {
    _read { yield self[AnyPatternID(p), default: defaultProperty()] }
    _modify { yield &self[AnyPatternID(p), default: defaultProperty()] }
  }

}
