/// Returns the result of calling `action` with an immutable projection of `value`.
public func read<T, U>(_ value: T, _ action: @Sendable (T) throws -> U) rethrows -> U {
  try action(value)
}

// Same as above but allows passing mutable state to the action.
public func read<T, U, S: Sendable>(_ value: T, withState s: inout S, _ action: @Sendable (inout S, T) throws -> U) rethrows -> U {
  try action(&s, value)
}


/// Returns the result of calling `action` with a mutable projection of `value`.
public func modify<T, U>(_ value: inout T, _ action: (inout T) throws -> U) rethrows -> U {
  try action(&value)
}

// Same as above but allows passing mutable state to the action.
public func modify<T, U, S: Sendable>(_ value: inout T, withState s: inout S, _ action: @Sendable (inout S, inout T) throws -> U) rethrows -> U {
  try action(&s, &value)
}

/// Assigns `value` to the result of applying `transform` on it.
public func update<T>(_ value: inout T, with transform: (T) throws -> T) rethrows {
  value = try transform(value)
}
