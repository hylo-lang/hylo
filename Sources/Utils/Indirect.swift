import Foundation

/// A wrapper type allocating `T` out-of-line.
public struct Indirect<T> {

  /// The out of line storage of an `Indirect` value.
  fileprivate class Storage {

    /// The payload of an `Indirect` value.
    var payload: T

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

  /// Accesses the wrapped value using copy on write
  public var value: T {
    _read {
      yield storage.payload
    }
    _modify {
      if isKnownUniquelyReferenced(&storage) {
        yield &storage.payload // Mutate in-place
      } else {
        // Perform a copy-on-write, saving the modified value into new, independent storage.
        var s = storage.payload
        yield &s
        storage = .init(payload: s)
      }
    }
  }
}

/// Protected by copy on write in Indirect's value accessor
extension Indirect: @unchecked Sendable where T: Sendable {}

extension Indirect: Equatable where T: Equatable {

  public static func == (l: Self, r: Self) -> Bool {
    (l.storage === r.storage) || (l.storage.payload == r.storage.payload)
  }

}

extension Indirect: Hashable where T: Hashable {

  public func hash(into hasher: inout Hasher) {
    hasher.combine(storage.payload)
  }

}

extension Indirect: Comparable where T: Comparable {

  public static func < (l: Self, r: Self) -> Bool {
    l.storage.payload < r.storage.payload
  }

}

extension Indirect: CustomStringConvertible where T: CustomStringConvertible {

  public var description: String {
    storage.payload.description
  }

}
