/// A lazily computed value.
///
/// The value is not computed unless accessed and is computed no more than once no matter how many
/// times it is copied or accessed.
public struct Lazy<T: Sendable>: Sendable {

  /// The hidden state of an instance.
  enum State: Sendable {
    /// The value has never been read; the payload is the value computation.
    case unevaluated(@Sendable () -> T)

    /// The value has been read and is stored in the payload.
    case result(T)
  }

  /// The hidden state of an instance.
  private let state: SharedMutable<State>

  /// Creates an instance whose value is `computeValue()`.
  ///
  /// - Requires: `computeValue` does not mutate any existing state.
  /// - Warning: Swift silently creates mutable captures in closures! If `computeValue` mutates
  ///   anything other than its local variables, you can create data races and undefined behavior.
  public init(_ computeValue: @Sendable @escaping () -> T) {
    state = .init(.unevaluated(computeValue))
  }

  /// The (lazily) computed value.
  public subscript() -> T {
    state.modify { s in
      switch s {
      case .result(let r): return r
      case .unevaluated(let f):
        let r = f()
        s = .result(r)
        return r
      }
    }
  }

}

/// A lazily computed value whose computation might throw.
///
/// The actual value is not computed unless accessed and is computed no more than once no matter how
/// many times this value is copied or accessed.  If the value computation throws, each access to
/// this value (or copy of it) throws the same error as the original access.
public struct LazyThrowing<T: Sendable> {

  /// The hidden state of an instance.
  private enum State: Sendable {
    /// The value has never been read; the payload is the value computation.
    case unevaluated(@Sendable () throws -> T)

    /// The value has been read and is stored in the payload.
    case success(T)

    /// An error was thrown from value computation and is stored in the payload.
    case failure(any Error)
  }

  /// The hidden state of an instance.
  private let state: SharedMutable<State>

  /// Creates an instance whose value is `computeValue()`.
  ///
  /// - Requires: `computeValue` does not mutate any existing state.
  /// - Warning: Swift silently creates mutable captures in closures! If `computeValue` mutates
  ///   anything other than its local variables, you can create data races and undefined behavior.
  public init(_ computeValue: @Sendable @escaping () throws -> T) {
    state = .init(.unevaluated(computeValue))
  }

  /// The (lazily) computed value.
  ///
  /// - Throws the error thrown by the computation if any.
  public subscript() -> T {
    get throws {
      try state.modify { s in
        switch s {
        case .success(let r): return r
        case .failure(let e): throw e
        case .unevaluated(let f):
          do {
            let r = try f()
            s = .success(r)
            return r
          } catch let e {
            s = .failure(e)
            throw e
          }
        }
      }
    }
  }

}
