import Core

/// The result of a Val test run.
struct ValTestRunResult {

  /// Indicates whether the test case ran through completion without any compilation error.
  let ranToCompletion: Bool

  /// The diagnostics reported throughout compilation.
  let diagnostics: [Diagnostic]

}
