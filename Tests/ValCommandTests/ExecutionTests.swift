import ArgumentParser
import TestUtils
import ValCommand
import XCTest

final class ExecutionTests: XCTestCase {

  /// The result of a compiler invokation.
  private struct CompilationResult {

    /// The exit status of the compiler.
    let status: ExitCode

    /// The URL of the output file.
    let output: URL

    /// The contents of the standard error.
    let stderr: String

  }

  func testExecution() throws {
    try checkAnnotatedValFiles(
      in: Bundle.module.url(forResource: "TestCases/Execution", withExtension: nil)!,
      checkingAnnotationCommands: ["run"],
      { (source, cxxAnnotations, diagnostics) in
        // Compile the test file; expect success.
        let result = try compile(source.url, with: ["--emit", "binary"])
        XCTAssert(result.status.isSuccess)
        XCTAssert(result.stderr.isEmpty)
        XCTAssert(FileManager.default.fileExists(atPath: result.output.relativePath))

        return cxxAnnotations.compactMap { a in
          // Run the executable.
          let task = Process()
          task.executableURL = result.output
          if (try? task.run()) == nil {
            return a.failure("Cannot execute: \(result.output)")
          }
          task.waitUntilExit()

          // Check the termination status of the executed process.
          XCTAssert(task.terminationStatus == 0)
          return nil
        }
      })
  }

  /// Compiles `input` with the given arguments and returns the compiler's exit status, the URL of
  /// the output file, and the contents of the standard error.
  private func compile(
    _ input: URL,
    with arguments: [String]
  ) throws -> CompilationResult {
    // Create a temporary directory to write the output file.
    let outputDirectory = try FileManager.default.url(
      for: .itemReplacementDirectory,
      in: .userDomainMask,
      appropriateFor: input,
      create: true)
    let output = outputDirectory.appendingPathComponent("a.out")

    // Parse the command line's arguments.
    let cli = try ValCommand.parse(arguments + ["-o", output.relativePath, input.relativePath])

    // Execute the command.
    var stderr = ""
    let status = try cli.execute(loggingTo: &stderr)
    return CompilationResult(status: status, output: output, stderr: stderr)
  }

}
