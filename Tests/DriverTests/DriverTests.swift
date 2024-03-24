import ArgumentParser
import Core
import Driver
import TestUtils
import Utils
import XCTest

final class DriverTests: XCTestCase {

  /// The result of a compiler invocation.
  private struct CompilationResult {

    /// The exit status of the compiler.
    let status: ExitCode

    /// The URL of the output file.
    let output: URL

    /// The text of generated diagnostics.
    let diagnosticText: String

    /// `XCTAssert`'s that `diagnosticText` matches `expectedDiagnostics`, with better output
    /// formatting on failure.
    func checkDiagnosticText(
      `is` expectedDiagnostics: String, file: StaticString = #filePath, line: UInt = #line
    ) {
      XCTAssertEqual("\n" + diagnosticText, "\n" + expectedDiagnostics, file: file, line: line)
    }
  }

  func testNoInput() throws {
    let cli = try Driver.parse([])
    XCTAssertThrowsError(try cli.execute())
  }

  func testRawAST() throws {
    let result = try compile(["--emit", "raw-ast"], newFile(containing: "public fun main() {}"))
    XCTAssert(result.status.isSuccess)
    XCTAssert(FileManager.default.fileExists(atPath: result.output.relativePath))
    result.checkDiagnosticText(is: "")
  }

  func testRawIR() throws {
    let result = try compile(["--emit", "raw-ir"], newFile(containing: "public fun main() {}"))
    XCTAssert(result.status.isSuccess)
    result.checkDiagnosticText(is: "")
    XCTAssert(FileManager.default.fileExists(atPath: result.output.relativePath))
  }

  func testIR() throws {
    let result = try compile(["--emit", "ir"], newFile(containing: "public fun main() {}"))
    XCTAssert(result.status.isSuccess)
    result.checkDiagnosticText(is: "")
    XCTAssert(FileManager.default.fileExists(atPath: result.output.relativePath))
  }

  func testLLVM() throws {
    if swiftyLLVMMandatoryPassesCrash { return }

    let result = try compile(["--emit", "llvm"], newFile(containing: "public fun main() {}"))
    XCTAssert(result.status.isSuccess)
    result.checkDiagnosticText(is: "")
    XCTAssert(FileManager.default.fileExists(atPath: result.output.relativePath))
  }

  func testIntelASM() throws {
    if swiftyLLVMMandatoryPassesCrash { return }

    let result = try compile(["--emit", "intel-asm"], newFile(containing: "public fun main() {}"))
    XCTAssert(result.status.isSuccess)
    result.checkDiagnosticText(is: "")
    XCTAssert(FileManager.default.fileExists(atPath: result.output.relativePath))
  }

  func testBinary() throws {
    if swiftyLLVMMandatoryPassesCrash { return }

    let result = try compile(["--emit", "binary"], newFile(containing: "public fun main() {}"))
    XCTAssert(result.status.isSuccess)
    result.checkDiagnosticText(is: "")

    #if os(Windows)
      XCTAssert(FileManager.default.fileExists(atPath: result.output.relativePath + ".exe"))
    #else
      XCTAssert(FileManager.default.fileExists(atPath: result.output.relativePath))
    #endif
  }

  func testParseFailure() throws {
    let valSource = try newFile(containing: "fun x")
    let result = try compile([], valSource)
    XCTAssertFalse(result.status.isSuccess)
    result.checkDiagnosticText(
      is: """
        \(valSource.relativePath):1.6: error: expected function signature
        fun x
             ^

        """)
  }

//  func testTypeCheckSuccess() throws {
//    let result = try compile(["--typecheck"], newFile(containing: "public fun main() {}"))
//    XCTAssert(result.status.isSuccess)
//    result.checkDiagnosticText(is: "")
//    XCTAssertFalse(FileManager.default.fileExists(atPath: result.output.relativePath))
//  }

  func testTypeCheckFailure() throws {
    let valSource = try newFile(containing: "public fun main() { foo() }")
    let result = try compile(["--typecheck"], valSource)
    XCTAssertFalse(result.status.isSuccess)
    result.checkDiagnosticText(
      is: """
        \(valSource.relativePath):1.21-24: error: undefined name 'foo' in this scope
        public fun main() { foo() }
                            ~~~

        """)
    XCTAssertFalse(FileManager.default.fileExists(atPath: result.output.relativePath))
  }

  /// Writes `s` to a temporary file and returns its URL.
  private func newFile(containing s: String) throws -> URL {
    let f = FileManager.default.makeTemporaryFileURL()
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
    let output = FileManager.default.makeTemporaryFileURL()

    // Parse the command line's arguments.
    let cli = try Driver.parse(arguments + ["-o", output.relativePath, input.relativePath])

    // Execute the command.
    let (status, diagnostics) = try cli.execute()
    return CompilationResult(
      status: status, output: output, diagnosticText: diagnostics.rendered())
  }

}
