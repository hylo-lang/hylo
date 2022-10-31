import Compiler
import Foundation
import Utils

/// A helper for loading and executing test cases.
struct TestCase {

  /// The source file defining the test case.
  var source: SourceFile

  /// The test annotations of the test case.
  var annotations: [TestAnnotation]

  /// Loads a test case from `source`.
  init(source: SourceFile) {
    self.source = source
    self.annotations = TestAnnotation.parseAll(from: source)
  }

  /// The name of the test case.
  var name: String { source.url.deletingPathExtension().lastPathComponent }

  /// Loads all the test cases in `directory` and executes them with the specified handler.
  static func executeAll(in directory: URL, _ handler: (Self) throws -> Void) throws {
    try withFiles(in: directory, { url in
      try handler(TestCase(source: SourceFile(contentsOf: url)))
      return true
    })
  }

}
