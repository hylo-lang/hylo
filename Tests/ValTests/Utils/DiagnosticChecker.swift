import Compiler
import XCTest

/// A test annotation handler that checks if all expected diagnostics have been emitted.
struct DiagnosticChecker {

  /// The name of the test case for which diagnostics are being checked.
  var testCaseName: String

  /// A table mapping line locations to the diagnostics emitted at those locations.
  var emittedDiagnostics: [LineLocation?: [Diagnostic]]

  /// Creates a new checker for the given set of diagnostics.
  init<S: Sequence>(testCaseName: String, diagnostics: S) where S.Element == Diagnostic {
    self.testCaseName = testCaseName
    self.emittedDiagnostics = diagnostics.reduce(into: [:], { (ds, d) in
      ds[d.location.map(LineLocation.init), default: []].append(d)
    })
  }

  /// Handles a `diagnositc` test annotation.
  mutating func handle(
    _ annotation: TestAnnotation,
    file: StaticString = #filePath,
    line: UInt = #line
  ) {
    assert(annotation.command == "diagnostic")

    // Retrieve the list of expected diagnostics at the annotation's location.
    var ds = emittedDiagnostics[annotation.location, default: []]
    guard let i = ds.firstIndex(where: { $0.message == annotation.argument }) else {
      XCTFail(
        "\(testCaseName): missing expected diagnostic at \(annotation.location)",
        file: file,
        line: line)
      return
    }

    // Remove the diagnostic from the set.
    ds.remove(at: i)
    emittedDiagnostics[annotation.location] = ds.isEmpty ? nil : ds
  }

  /// Checks that all emitted diagnostics were expected.
  func finalize(file: StaticString = #filePath, line: UInt = #line) {
    XCTAssert(
      emittedDiagnostics.isEmpty,
      "\(testCaseName): \(emittedDiagnostics.count) unexpected diagnostic(s)",
      file: file,
      line: line)
  }

}
