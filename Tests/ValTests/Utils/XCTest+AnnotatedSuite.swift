import Core
import TestUtils
import XCTest

extension XCTestCase {

  /// The URL of the `ValTests/` directory of this project.
  fileprivate static let valTests = URL(fileURLWithPath: String(#filePath))
    .deletingLastPathComponent()
    .deletingLastPathComponent()

  /// Applies `process` to each ".val" file in the test suite at `suitePath` (relative to the
  /// `ValTests/` directory of this project) and reports XCTest failures where the effects of
  /// processing don't match the file's annotation commands ("diagnostic", "expect-failure", and
  /// "expect-success").
  ///
  /// - Parameter process: applies some compilation phases to `file`, updating `diagnostics`
  ///   with any generated diagnostics. Throws an `Error` if any phases failed.
  func checkAnnotatedValFileDiagnostics(
    inSuiteAt suitePath: String,
    _ process: (_ file: SourceFile, _ diagnostics: inout DiagnosticSet) throws -> Void
  ) throws {
    let annotatedValFiles = try testSuite(at: suitePath)
    for f in annotatedValFiles {
      try checkAnnotations(
        in: f, checkingAnnotationCommands: [],
        { (file, annotationsToHandle, diagnostics) in
          assert(annotationsToHandle.isEmpty)
          try process(file, &diagnostics)
          return []
        }
      )
    }
  }

  /// Returns the Val test cases in the suite at `path` relative to the `ValTests/` directory of
  /// this project.
  ///
  /// The suite is sought at `path` relative to `ValTests/`. If no such directory exists, the
  /// suite is sought at `path` relative to the root of the resource bundle associated with the
  /// current Swift module.
  fileprivate func testSuite(
    at path: String
  ) throws -> [SourceFile] {
    let p = URL(fileURLWithPath: path, relativeTo: XCTestCase.valTests)
    if p.hasDirectoryPath {
      return try sourceFiles(in: [p])
    } else {
      let s = Bundle.module.url(forResource: path, withExtension: nil)!
      return try sourceFiles(in: [s])
    }
  }
}
