import ArgumentParser
import ValCommand
import XCTest

final class ValCommandTests: XCTestCase {

  /// The result of a compiler invokation.
  private struct CompilationResult {

    /// The exit status of the compiler.
    let status: ExitCode

    /// The URL of the output file.
    let output: URL

    /// The contents of the standard error.
    let stderr: String

  }

  func testNoInput() throws {
    XCTAssertThrowsError(try ValCommand.parse([]))
  }

  func testRawAST() throws {
    let input = try url(forSourceNamed: "Success")
    let result = try compile(input, with: ["--emit", "raw-ast"])
    XCTAssert(result.status.isSuccess)
    XCTAssert(FileManager.default.fileExists(atPath: result.output.relativePath))
    XCTAssert(result.stderr.isEmpty)
  }

  func testRawIR() throws {
    let input = try url(forSourceNamed: "Success")
    let result = try compile(input, with: ["--emit", "raw-ir"])
    XCTAssert(result.status.isSuccess)
    XCTAssert(result.stderr.isEmpty)
    XCTAssert(FileManager.default.fileExists(atPath: result.output.relativePath))
  }

  func testIR() throws {
    let input = try url(forSourceNamed: "Success")
    let result = try compile(input, with: ["--emit", "ir"])
    XCTAssert(result.status.isSuccess)
    XCTAssert(result.stderr.isEmpty)
    XCTAssert(FileManager.default.fileExists(atPath: result.output.relativePath))
  }

  func testCPP() throws {
    let input = try url(forSourceNamed: "Success")
    let result = try compile(input, with: ["--emit", "cpp"])
    XCTAssert(result.status.isSuccess)
    XCTAssert(result.stderr.isEmpty)

    let baseURL = result.output.deletingPathExtension()
    XCTAssert(
      FileManager.default.fileExists(atPath: baseURL.appendingPathExtension("h").relativePath))
    XCTAssert(
      FileManager.default.fileExists(atPath: baseURL.appendingPathExtension("cpp").relativePath))
  }

  #if !os(Windows)
    func testBinary() throws {
      let input = try XCTUnwrap(
        Bundle.module.url(forResource: "Success", withExtension: ".val", subdirectory: "Inputs"),
        "No inputs")

      let result = try compile(input, with: ["--emit", "binary"])
      XCTAssert(result.status.isSuccess)
      XCTAssert(result.stderr.isEmpty)
      XCTAssert(FileManager.default.fileExists(atPath: result.output.relativePath))
    }
  #endif

  func testTypeCheckSuccess() throws {
    let input = try url(forSourceNamed: "Success")
    let result = try compile(input, with: ["--typecheck"])
    XCTAssert(result.status.isSuccess)
    XCTAssert(result.stderr.isEmpty)
  }

  func testTypeCheckFailure() throws {
    let input = try url(forSourceNamed: "Failure")
    let result = try compile(input, with: ["--typecheck"])
    XCTAssertFalse(result.status.isSuccess)

    let expectedStandardError = """
      \(input.relativePath):2:11: error: undefined name 'foo' in this scope
        let x = foo()
                ~~~

      """
    XCTAssertEqual(expectedStandardError, result.stderr)
  }

  /// Returns the URL of the Val source file named `n`.
  private func url(
    forSourceNamed n: String,
    file: StaticString = #file,
    line: UInt = #line
  ) throws -> URL {
    try XCTUnwrap(
      Bundle.module.url(forResource: n, withExtension: ".val", subdirectory: "Inputs"),
      "No inputs",
      file: file, line: line)
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

extension String: Log {

  public var hasANSIColorSupport: Bool { false }

}
