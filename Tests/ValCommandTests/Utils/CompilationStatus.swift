import ArgumentParser
import XCTest

/// The result of a compiler invokation.
struct CompilationResult {

  /// The exit status of the compiler.
  let status: ExitCode

  /// The URL of the output file.
  let output: URL

  /// The contents of the standard error.
  let stderr: String

  /// Check that the compilation was successful.
  func assertSuccess(checkOutput: Bool = true) {
    XCTAssert(status.isSuccess, "Compilation failed with exit code \(status)")
    XCTAssert(stderr.isEmpty, "Compilation contains errors: \(stderr)")
    if checkOutput {
      XCTAssert(
        FileManager.default.fileExists(atPath: output.relativePath),
        "Compilation output file not found: \(output.relativePath)")
    }
  }

}
