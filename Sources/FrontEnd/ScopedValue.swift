/// A value bundled with a lexical scope.
public struct ScopedValue<Value> {

  /// The context in which `value` is used.
  public let scope: AnyScopeID

  /// A value.
  public let value: Value

  /// Creates an instance bundling `value` with `scope`.
  public init(_ value: Value, in scope: AnyScopeID) {
    self.scope = scope
    self.value = value
  }

}

extension ScopedValue: Equatable where Value: Equatable {}

extension ScopedValue: Hashable where Value: Hashable {}
