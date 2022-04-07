extension Optional {

  /// Returns the wrapped value, assuming it exists, leaving this optional empty.
  public mutating func release() -> Wrapped {
    defer { self = nil }
    return self!
  }

}
