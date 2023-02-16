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

  /// Compiles `input` with the given arguments and returns the URL of the output file.
  ///
  /// Ensures that the compilation succeeds.
  func compile(_ input: URL, with arguments: [String]) throws -> URL {
    let output = FileManager.default.temporaryFile()
    let cli = try ValCommand.parse(arguments + ["-o", output.relativePath, input.relativePath])
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
