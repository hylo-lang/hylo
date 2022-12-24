import Core
import Utils
import XCTest

/// A XCTestCase that runs val test cases.
protocol ValTestRunner: XCTestCase {

  /// The directory containing the Val test cases of this type.
  static var testCaseDirectoryPath: String { get }

}

extension ValTestRunner {

  /// Runs the test cases found in `Self.testCaseDirectoryPath` using the given closure.
  func runValTests(_ run: (_ name: String, _ source: SourceFile) -> ValTestRunResult) throws {
    let testCaseDirectory = try XCTUnwrap(
      Bundle.module.url(forResource: Self.testCaseDirectoryPath, withExtension: nil),
      "No test cases")

    func _run(name: String, url: URL, activity: XCTActivity) throws -> [XCTIssue] {
      // Run the test case.
      let source = try SourceFile(contentsOf: url)
      let result = run(name, source)

      // Handle the test annotations.
      var handler = DefaultTestAnnotationHandler(
        testCaseRanToCompletion: result.ranToCompletion, diagnostics: result.diagnostics)
      handler.handle(TestAnnotation.parseAll(from: source))
      return handler.finalize()
    }

    try withFiles(in: testCaseDirectory, { (url) in
      // Skip non-val files.
      if url.pathExtension != "val" { return true }

      // Create an activity encapsulating the current test case.
      let name = url.deletingPathExtension().lastPathComponent
      let issues = try XCTContext.runActivity(
        named: name,
        block: { (activity) in try _run(name: name, url: url, activity: activity) })

      for issue in issues {
        record(issue)
      }

      // Move to the next test case.
      return true
    })
  }

}
