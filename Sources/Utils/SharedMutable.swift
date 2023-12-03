import Dispatch

/// A threadsafe shared mutable wrapper for a `SharedValue` instance.
///
/// - Warning: The shared value has reference semantics; it's up to the programmer to ensure that
/// the value only used monotonically.
public final class SharedMutable<SharedValue> {

  /// The synchronization mechanism that makes `self` threadsafe.
  private let mutex = DispatchQueue(label: "org.hylo-lang.\(SharedValue.self)")

  /// The (thread-unsafe) stored instance.
  private var storage: SharedValue

  /// Creates an instance storing `toBeShared`.
  public init(_ toBeShared: SharedValue) {
    self.storage = toBeShared
  }

  /// Thread-safely accesses the wrapped instance.
  public subscript() -> SharedValue {
    get { mutex.sync { storage } }
    set { mutex.sync { storage = newValue } }
  }

  /// Returns the result of thread-safely applying `modification` to the wrapped instance.
  ///
  /// - Requires: `computeValue` does not mutate any existing state.
  /// - Warning: Swift silently creates mutable captures in closures! If `computeValue` mutates
  ///   anything other than its local variables, you can create data races and undefined behavior.
  public func modify<R>(applying modification: (inout SharedValue) throws -> R) rethrows -> R {
    try mutex.sync {
      let r = try modification(&storage)
      return r
    }
  }

}
