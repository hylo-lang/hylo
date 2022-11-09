/// A value that represents either a success or a failure with its diagnostics.
public typealias FallibleWithDiagnostic<T> = Result<T, DiagnosedError>

extension Result where Failure == DiagnosedError {

  /// The diagnostics in the payload of `.failure`. Otherwise, an empty array.
  public var diagnostics: [Diagnostic] {
    if case .failure(let error) = self {
      return error.diagnostics
    } else {
      return []
    }
  }

}
