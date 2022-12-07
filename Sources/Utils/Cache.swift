/// A cache of instances of `Value` keyed by instances of `Key`.
public struct Cache<Key: Hashable, Value> {

  /// An error thrown when `Cache` detects infinite recursion.
  public struct InfiniteRecursionError: Error {

    /// The key for which a value computation caused infinite recursion.
    let key: Key

  }

  /// The internal storage of the cache.
  private var storage: [Key: MemoizationState<Value>] = [:]

  /// Creates an empty cache.
  public init() {}

  /// Returns whether a value is currently being computed for `key`.
  public func isValueBeingComputed(forKey key: Key) -> Bool {
    if case .some(.inProgress) = storage[key] {
      return true
    } else {
      return false
    }
  }

  /// Returns the value associated with `key`, if any.
  public func value(forKey key: Key) -> Value? {
    if case .done(let value) = storage[key] {
      return value
    } else {
      return nil
    }
  }

  /// Returns the value associated with `key` in the cache. Otherwise, calls `computeIfMissing`
  /// with `key`, caches its result and returns it.
  public mutating func value(
    forKey key: Key,
    computedBy computeIfMissing: (Key) throws -> Value
  ) rethrows -> Value {
    try modifying(&storage[key, default: .inProgress], { value in
      switch value {
      case .inProgress:
        let result = try computeIfMissing(key)
        value = .done(result)
        return result

      case .done(let result):
        return result
      }
    })
  }

  /// Returns the value associated with `key` in the cache. Otherwise, calls `computeIfMissing`
  /// with a projection of `self` and `key`, caches its result and returns it.
  ///
  /// - Throws: `InfiniteRecursionError` if `computeIfMissing` causes infinite recursion.
  public mutating func value(
    forKey key: Key,
    computedByReentrant computeIfMissing: (inout Self, Key) throws -> Value
  ) throws -> Value {
    switch storage[key] {
    case .done(let value):
      return value

    case .inProgress:
      throw InfiniteRecursionError(key: key)

    default:
      storage[key] = .inProgress
      let value = try computeIfMissing(&self, key)
      storage[key] = .done(value)
      return value
    }
  }

  /// Accesses the value of the cache at `key`.
  public subscript(key: Key) -> MemoizationState<Value>? {
    get     { storage[key] }
    _modify { yield &storage[key] }
  }

}

extension Cache: Collection {

  public typealias Index = Dictionary<Key, MemoizationState<Value>>.Index

  public typealias Element = Dictionary<Key, MemoizationState<Value>>.Element

  public var startIndex: Index { storage.startIndex }

  public var endIndex: Index { storage.endIndex }

  public func index(after i: Index) -> Index {
    storage.index(after: i)
  }

  public subscript(position: Index) -> Element {
    storage[position]
  }

}
