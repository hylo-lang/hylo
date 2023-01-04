import Core
import Utils
import XCTest

extension XCTestCase {

  /// Preprocesses the ".val" files in a directory using `preprocess`, checking results using the
  /// returned `Handler` to handle annotations embedded in the file's comments.
  func checkAnnotatedValFiles<Handler: TestAnnotationHandler>(
    in sourceDirectory: String,
    _ preprocess: (_ xcTestName: String, _ source: SourceFile) throws -> Handler
  ) throws {
    let testCaseDirectory = try XCTUnwrap(
      Bundle.module.url(forResource: sourceDirectory, withExtension: nil),
      "No test cases")

    func _run(xcTestName: String, url: URL, activity: XCTActivity) throws -> [XCTIssue] {
      // Run the test case.
      let source = try SourceFile(contentsOf: url)
      var handler = try preprocess(xcTestName, source)
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
