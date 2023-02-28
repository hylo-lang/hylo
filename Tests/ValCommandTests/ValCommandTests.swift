import ArgumentParser
import Utils
import ValCommand
import XCTest

final class ValCommandTests: XCTestCase {

  /// The result of a compiler invokation.
  private struct CompilationResult {

    /// The exit status of the compiler.
    let status: ExitCode

    /// The file or directory that was written by the compiler.
    let outputFile: URL

    /// The contents of the standard error.
    let stderr: String

  }

  func testNoInput() throws {
    XCTAssertThrowsError(try ValCommand.parse([]))
  }

  func testRawAST() throws {
    let result = try compile(["--emit", "raw-ast"], newFile(containing: "public fun main() {}"))
    XCTAssert(result.status.isSuccess)
    XCTAssert(FileManager.default.fileExists(atPath: result.outputFile.relativePath))
    XCTAssert(result.stderr.isEmpty)
  }

  func testRawIR() throws {
    let result = try compile(["--emit", "raw-ir"], newFile(containing: "public fun main() {}"))
    XCTAssert(result.status.isSuccess)
    XCTAssert(result.stderr.isEmpty)
    XCTAssert(FileManager.default.fileExists(atPath: result.outputFile.relativePath))
  }

  func testIR() throws {
    let result = try compile(["--emit", "ir"], newFile(containing: "public fun main() {}"))
    XCTAssert(result.status.isSuccess)
    XCTAssert(result.stderr.isEmpty)
    XCTAssert(FileManager.default.fileExists(atPath: result.outputFile.relativePath))
  }

  func testCPP() throws {
    let result = try compile(["--emit", "cpp"], newFile(containing: "public fun main() {}"))
    XCTAssert(result.status.isSuccess)
    XCTAssert(result.stderr.isEmpty)

    let baseURL = result.outputFile.deletingPathExtension()
    XCTAssert(
      FileManager.default.fileExists(atPath: baseURL.appendingPathExtension("h").relativePath))
    XCTAssert(
      FileManager.default.fileExists(atPath: baseURL.appendingPathExtension("cpp").relativePath))
  }

  func testBinary() throws {
    let result = try compile(["--emit", "binary"], newFile(containing: "public fun main() {}"))
    XCTAssert(result.status.isSuccess)
    XCTAssert(result.stderr.isEmpty)

    #if os(Windows)
      XCTAssert(FileManager.default.fileExists(atPath: result.outputFile.relativePath + ".exe"))
    #else
      XCTAssert(FileManager.default.fileExists(atPath: result.outputFile.relativePath))
    #endif
  }

  func testBinaryWithCCFlags() throws {
    let result = try compile(
      ["--emit", "binary", "--cc-flags", "O3"], newFile(containing: "public fun main() {}"))
    XCTAssert(result.status.isSuccess)
    XCTAssert(result.stderr.isEmpty)

    #if os(Windows)
      XCTAssert(FileManager.default.fileExists(atPath: result.output.relativePath + ".exe"))
    #else
      XCTAssert(FileManager.default.fileExists(atPath: result.outputFile.relativePath))
    #endif
  }

  func testParseFailure() throws {
    let valSource = try newFile(containing: "fun x")
    let result = try compile([], valSource)
    XCTAssertFalse(result.status.isSuccess)
    XCTAssertEqual(
      result.stderr,
      """
      \(valSource.relativePath):1:6: error: expected function signature
      fun x
           ^

      """)
  }

  func testTypeCheckSuccess() throws {
    let result = try compile(["--typecheck"], newFile(containing: "public fun main() {}"))
    XCTAssert(result.status.isSuccess)
    XCTAssert(result.stderr.isEmpty)
    XCTAssertFalse(FileManager.default.fileExists(atPath: result.outputFile.relativePath))
  }

  func testTypeCheckFailure() throws {
    let valSource = try newFile(containing: "public fun main() { foo() }")
    let result = try compile(["--typecheck"], valSource)
    XCTAssertFalse(result.status.isSuccess)
    XCTAssertEqual(
      result.stderr,
      """
      \(valSource.relativePath):1:21: error: undefined name 'foo' in this scope
      public fun main() { foo() }
                          ~~~

      """)
    XCTAssertFalse(FileManager.default.fileExists(atPath: result.outputFile.relativePath))
  }

  /// Writes `s` to a temporary file and returns its URL.
  private func newFile(containing s: String) throws -> URL {
    let f = FileManager.default.temporaryFile()
    try s.write(to: f, atomically: true, encoding: .utf8)
    return f
  }

  /// Compiles `input` with the given command line arguments and returns the compiler's exit
  /// status, the URL of the output file, and the contents of the standard error.
  private func compile(
    _ arguments: [String],
    _ input: URL
  ) throws -> CompilationResult {
    // Create a temporary output.
    let output = FileManager.default.temporaryFile()

    // Parse the command line's arguments.
    let cli = try ValCommand.parse(arguments + ["-o", output.relativePath, input.relativePath])

    // Execute the command.
    var stderr = ""
    let status = try cli.execute(loggingTo: &stderr)
    return CompilationResult(status: status, outputFile: output, stderr: stderr)
  }

}

extension FileManager {

  /// Returns the URL of a temporary file.
  func temporaryFile() -> URL {
    temporaryDirectory.appendingPathComponent("\(UUID())")
  }

}

extension String: Log {

  public var hasANSIColorSupport: Bool { false }

}
