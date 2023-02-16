import ArgumentParser
import TestSupport
import ValCommand
import XCTest

final class ExecutionTests: XCTestCase {

  func testExecution() throws {
    try checkAnnotatedValFiles(
      in: Bundle.module.url(forResource: "TestCases", withExtension: nil)!,
      checkingAnnotationCommands: ["run"],
      { (source, cxxAnnotations, diagnostics) in
        // Compile the test file; expect success.
        let output = try compile(source.url, with: ["--emit", "binary"])

        return cxxAnnotations.compactMap { a in
          // Run the executable, check for success.
          return a.run(output)
        }
      })
  }

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

  /// Compiles `input` with the given arguments and returns the URL of the output file.
  ///
  /// Ensures that the compilation succeeds.
  func compile(_ input: URL, with arguments: [String]) throws -> URL {
    // Create a temporary output.
    let output = FileManager.default.temporaryFile()

    // Parse the command line's arguments.
    let cli = try ValCommand.parse(arguments + ["-o", output.relativePath, input.relativePath])

    // Execute the command.
    var stderr = ""
    let status = try cli.execute(loggingTo: &stderr)

    XCTAssert(status.isSuccess, "Compilation failed with exit code \(status)")
    XCTAssert(stderr.isEmpty, "Compilation contains errors: \(stderr)")
    XCTAssert(
      FileManager.default.fileExists(atPath: output.relativePath),
      "Compilation output file not found: \(output.relativePath)")

    return output
  }

}
