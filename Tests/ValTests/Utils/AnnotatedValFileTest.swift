import Core
import Utils
import XCTest

/// A XCTestCase that preprocesses the ".val" files in a directory, and then checks the result of
/// that preprocessing against annotations embedded in the file's comments.
protocol AnnotatedValFileTest: XCTestCase {

  /// The directory containing the Val sources to test.
  static var valSourceDirectory: String { get }

}

extension AnnotatedValFileTest {

  /// Runs the test cases found in `Self.valSourceDirectory` with `runSteps` and handle the
  /// result of each case with an instance of `handler`.
  func checkAnnotatedValFiles<Handler: TestAnnotationHandler>(
    _ runSteps: (_ xcTestName: String, _ source: SourceFile) throws -> Handler
  ) throws {
    let testCaseDirectory = try XCTUnwrap(
      Bundle.module.url(forResource: Self.valSourceDirectory, withExtension: nil),
      "No test cases")

    func _run(xcTestName: String, url: URL, activity: XCTActivity) throws -> [XCTIssue] {
      // Run the test case.
      let source = try SourceFile(contentsOf: url)
      var handler = try runSteps(xcTestName, source)
      return handler.handles(TestAnnotation.parseAll(from: source))
    }

    try withFiles(
      in: testCaseDirectory,
      { (url) in
        // Skip non-val files.
        if url.pathExtension != "val" { return true }

        // Create an activity encapsulating the current test case.
        let xcTestName = url.deletingPathExtension().lastPathComponent
        let issues = try XCTContext.runActivity(
          named: xcTestName,
          block: { (activity) in try _run(xcTestName: xcTestName, url: url, activity: activity) })

        for issue in issues {
          record(issue)
        }

        // Move to the next test case.
        return true
      })
  }

}
