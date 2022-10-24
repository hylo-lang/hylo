import Compiler
import Foundation

/// A helper for loading and executing test cases.
struct TestCase {

  /// The URL of the test case.
  let url: URL

  /// The name of the test case.
  var name: String { url.deletingPathExtension().lastPathComponent }

  /// Loads this test case and executes it with the specified handler.
  ///
  /// - Parameter handler: A closure that accepts a source file with a set of test annotations.
  func execute(_ handler: (SourceFile, [TestAnnotation]) throws -> Void) throws {
    let input = try SourceFile(contentsOf: url)
    let annotations = try TestAnnotation.parseAll(from: input)
    try handler(input, annotations)
  }

}
