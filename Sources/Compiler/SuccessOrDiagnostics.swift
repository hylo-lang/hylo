/// A value that represents either a success or a failure with its diagnostics.
public enum SuccessOrDiagnostics {

  /// A success.
  case success

  /// A failure with its diagnostics.
  case failure([Diagnostic])

  /// The diagnostics in the payload of `.failure`. Otherwise, an empty array.
  public var diagnostics: [Diagnostic] {
    if case .failure(let ds) = self { return ds } else { return [] }
  }

}
