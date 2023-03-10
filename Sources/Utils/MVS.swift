/// Returns the result of calling `action` with an immutable projection of `value`.
public func read<T, U>(_ value: T, _ action: (T) throws -> U) rethrows -> U {
  try action(value)
}

/// Returns the result of calling `action` with a mutable projection of `value`.
public func modify<T, U>(_ value: inout T, _ action: (inout T) throws -> U) rethrows -> U {
  try action(&value)
}

/// Returns a copy of `value` modified by `transform`.
///
/// Use this function to initialize values with an imperative algorithm. For example:
///
///      let sieve = modified(Array(repeating: true, count: 100)) { (n) in
///        var i = 2
///        while i * i < n.count {
///          if n[i] {
///            for j in stride(from: i * i, to: n.count, by: i) { n[j] = false }
///          }
///          i += 1
///        }
///      }
public func modified<T>(_ value: T, _ transform: (inout T) throws -> Void) rethrows -> T {
  var v = value
  try transform(&v)
  return v
}

/// Assigns `value` to the result of applying `transform` on it.
public func assign<T>(_ value: inout T, to transform: (T) throws -> T) rethrows {
  value = try transform(value)
}
