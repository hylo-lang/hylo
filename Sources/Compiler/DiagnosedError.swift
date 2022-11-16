/// An error with one or more diagnostics.
public struct DiagnosedError: Error {

  /// The diagnostics of the error.
  public let diagnostics: [Diagnostic]

  /// Creates a new instance with the given diagnostic.
  public init(_ d: Diagnostic) {
    self.diagnostics = [d]
  }

  /// Creates a new instance with the given diagnostics.
  ///
  /// - Requires: `ds` is not empty.
  public init(_ ds: [Diagnostic]) {
    precondition(!ds.isEmpty)
    self.diagnostics = ds
  }

}
