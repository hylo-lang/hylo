/// Returns the result of calling `action` with an immutable projection of `value`.
public func read<T, U>(_ value: T, _ action: (T) throws -> U) rethrows -> U {
  try action(value)
}

/// Returns the result of calling `action` with a mutable projection of `value`.
public func modifying<T, U>(_ value: inout T, _ action: (inout T) throws -> U) rethrows -> U {
  try action(&value)
}

/// Returns a copy of `value` modified by `transform`.
public func modified<T>(_ value: T, _ transform: (inout T) throws -> Void) rethrows -> T {
  var v = value
  try transform(&v)
  return v
}

/// Assigns `value` to the result of applying `transform` on it.
public func modify<T>(_ value: inout T, with transform: (T) throws -> T) rethrows {
  value = try transform(value)
}
