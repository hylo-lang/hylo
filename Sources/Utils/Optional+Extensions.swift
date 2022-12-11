extension Optional {

  /// Returns the wrapped value, assuming it exists, leaving this optional empty.
  public mutating func release() -> Wrapped {
    defer { self = nil }
    return self!
  }

  /// Evaluates the given closure when this Optional instance is not `nil`, passing the unwrapped
  /// value as a parameter. Otherwise, returns `defaultValue`.
  public func map<U>(default defaultValue: @autoclosure () -> U, _ transform: (Wrapped) -> U) -> U {
    map(transform) ?? defaultValue()
  }

}
