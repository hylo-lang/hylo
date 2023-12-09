/// A value of type `T` or the error produced by computing that value.
public enum ResultOrError<T> {

  /// The successful computation.
  case result(T)

  /// The error thrown by a failed computation.
  case error(any Error)

  /// The result of `constructResult()` or if it throws, the error it threw.
  public init(_ constructResult: () throws -> T) {
    do {
      self = .result(try constructResult())
    } catch let e {
      self = .error(e)
    }
  }

  /// The result of the successful computation if any, or `nil` otherwise.
  public var result: T? {
    if case .result(let r) = self { return r }
    return nil
  }

  /// The error thrown by the failed computation if any, or `nil` otherwise.
  public var error: (any Error)? {
    if case .error(let e) = self { return e }
    return nil
  }

  /// Accesses the result of the successful computation if any, or else rethrows the error thrown by
  /// the computation.
  public subscript() -> T {
    get throws {
      switch self {
      case .result(let r): return r
      case .error(let e): throw e
      }
    }
  }

}
