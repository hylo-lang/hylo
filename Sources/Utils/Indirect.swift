import Foundation

/// A wrapper type allocating `T` out-of-line.
public struct Indirect<T> {

  /// The out of line storage of an `Indirect` value.
  fileprivate class Storage {

    /// The payload of an `Indirect` value.
    var payload: T
    let lock = NSLock()

    /// Creates an instance wrapping `payload`.
    init(payload: T) {
      self.payload = payload
    }

  }

  /// The out-of-line storage of `self`.
  private var storage: Storage

  /// Creates an instance wrapping `value`.
  public init(_ value: T) {
    self.storage = .init(payload: value)
  }

  /// Accesses the wrapped value.
  public var value: T {
    _read {
      storage.lock.lock()
      defer { storage.lock.unlock() }
      yield storage.payload
    }
    _modify {
      if !isKnownUniquelyReferenced(&storage) {
        // Perform a copy-on-write.
        storage.lock.lock()
        let p = storage.payload
        storage.lock.unlock()
        storage = .init(payload: p)
      }

      storage.lock.lock()
      defer { storage.lock.unlock() }
      yield &storage.payload
    }
  }

}

extension Indirect: @unchecked Sendable where T: Sendable {}

extension Indirect: Equatable where T: Equatable {

  public static func == (l: Self, r: Self) -> Bool {
    if l.storage === r.storage { return true }

    l.storage.lock.lock()
    defer { l.storage.lock.unlock() }
    r.storage.lock.lock()
    defer { r.storage.lock.unlock() }
    return l.storage.payload == r.storage.payload
  }

}

extension Indirect: Hashable where T: Hashable {

  public func hash(into hasher: inout Hasher) {
    storage.lock.lock()
    defer { storage.lock.unlock() }
    hasher.combine(storage.payload)
  }

}

extension Indirect: Comparable where T: Comparable {

  public static func < (l: Self, r: Self) -> Bool {
    l.storage.lock.lock()
    defer { l.storage.lock.unlock() }
    r.storage.lock.lock()
    defer { r.storage.lock.unlock() }
    return l.storage.payload < r.storage.payload
  }

}

extension Indirect: CustomStringConvertible where T: CustomStringConvertible {

  public var description: String {
    storage.lock.lock()
    defer { storage.lock.unlock() }
    return storage.payload.description
  }

}
