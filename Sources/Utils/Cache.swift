/// A cache of instances of `Value` keyed by instances of `Key`.
public struct Cache<Key: Hashable, Value> {

  /// The internal storage of the cache.
  private var storage: [Key: Value] = [:]

  /// Creates an empty cache.
  public init() {}

  /// Returns the value associated with `key` in the cache. Otherwise, calls `computeIfMissing`,
  /// caches its result and returns it.
  public mutating func value(
    forKey key: Key,
    computedBy computeIfMissing: () throws -> Value
  ) rethrows -> Value {
    if let value = storage[key] {
      return value
    } else {
      let value = try computeIfMissing()
      storage[key] = value
      return value
    }
  }

}
