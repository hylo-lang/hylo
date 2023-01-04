import Core
import Utils
import XCTest

extension XCTestCase {

  /// Preprocesses the ".val" files in a directory using `preprocess`, checking results using the
  /// returned `Handler` to handle annotations embedded in the file's comments.
  func checkAnnotatedValFiles<Handler: TestAnnotationHandler>(
    in sourceDirectory: String,
    _ preprocess: (_ source: SourceFile) throws -> Handler
  ) throws {
    let testCaseDirectory = try XCTUnwrap(
      Bundle.module.url(forResource: sourceDirectory, withExtension: nil),
      "No test cases")

    try withFiles(
      in: testCaseDirectory,
      { (url) in
        // Skip non-val files.
        if url.pathExtension != "val" { return true }

        let source = try SourceFile(contentsOf: url)
        let issues = try XCTContext.runActivity(
          named: source.baseName,
          block: { activity in
            var handler = try preprocess(source)
            return handler.handles(TestAnnotation.parseAll(from: source))
          })

        for issue in issues {
          record(issue)
        }

        // Move to the next test case.
        return true
      })
  }

}
