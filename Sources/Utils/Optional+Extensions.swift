extension Optional {

  /// Returns the wrapped value, assuming it exists, leaving this optional empty.
  public mutating func release() -> Wrapped {
    defer { self = nil }
    return self!
  }

  /// If `self` is `nil`, wraps and returns `newValue`; returns the wrapped value otherwise.
  public mutating func setIfNil(
    _ newValue: @autoclosure () throws -> Wrapped
  ) rethrows -> Wrapped {
    if let v = self { return v }
    let v = try newValue()
    self = v
    return v
  }

  /// Evaluates the given closure when this Optional instance is not `nil`, passing the unwrapped
  /// value as a parameter; returns `defaultValue` otherwise.
  public func map<U>(
    default defaultValue: @autoclosure () -> U,
    _ transform: (Wrapped) -> U
  ) -> U {
    map(transform) ?? defaultValue()
  }

}
