import Dispatch

/// A threadsafe shared mutable wrapper for a `WrappedType` instance.
public class SharedMutable<WrappedType> {

  /// The synchronization mechanism that makes `self` threadsafe.
  private let mutex = DispatchQueue(label: "org.hylo-lang.\(WrappedType.self)")

  /// The (thread-unsafe) stored instance.
  private var storage: WrappedType

  /// Creates an instance storing `wrapped`.
  public init(_ wrapped: WrappedType) {
    self.storage = wrapped
  }

  /// Thread-safely accesses the wrapped instance.
  public var wrapped: WrappedType {
    get { mutex.sync { storage } }
    set { mutex.sync { storage = newValue } }
  }

  /// Returns the result of thread-safely applying `modification` to the wrapped instance.
  public func modify<R>(applying modification: (inout WrappedType) throws -> R) rethrows -> R {
    try mutex.sync {
      let r = try modification(&storage)
      return r
    }
  }

}
