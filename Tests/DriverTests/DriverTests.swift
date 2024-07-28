import ArgumentParser
import Driver
import FrontEnd
import TestUtils
import Utils
import XCTest

final class DriverTests: XCTestCase {

  func testNoInput() throws {
    let cli = try Driver.parse([])
    var log = DiagnosticSet()
    XCTAssertThrowsError(try cli.executeCommand(reportingDiagnosticsTo: &log))
    XCTAssert(log.isEmpty)
  }

  func testRawAST() throws {
    let result = try Driver.compileToTemporary(
      FileManager.default.temporaryFile(containing: "public fun main() {}"),
      withOptions: ["--emit", "raw-ast"])
    XCTAssert(result.status.isSuccess)
    XCTAssert(FileManager.default.fileExists(atPath: result.output.relativePath))
    result.checkDiagnosticText(is: "")
  }

  func testRawIR() throws {
    let result = try Driver.compileToTemporary(
      FileManager.default.temporaryFile(containing: "public fun main() {}"),
      withOptions: ["--emit", "raw-ir"])
    XCTAssert(result.status.isSuccess)
    result.checkDiagnosticText(is: "")
    XCTAssert(FileManager.default.fileExists(atPath: result.output.relativePath))
  }

  func testIR() throws {
    let result = try Driver.compileToTemporary(
      FileManager.default.temporaryFile(containing: "public fun main() {}"),
      withOptions: ["--emit", "ir"])
    XCTAssert(result.status.isSuccess)
    result.checkDiagnosticText(is: "")
    XCTAssert(FileManager.default.fileExists(atPath: result.output.relativePath))
  }

  func testLLVM() throws {
    if swiftyLLVMMandatoryPassesCrash { return }

    let result = try Driver.compileToTemporary(
      FileManager.default.temporaryFile(containing: "public fun main() {}"),
      withOptions: ["--emit", "llvm"])
    XCTAssert(result.status.isSuccess)
    result.checkDiagnosticText(is: "")
    XCTAssert(FileManager.default.fileExists(atPath: result.output.relativePath))
  }

  func testIntelASM() throws {
    if swiftyLLVMMandatoryPassesCrash { return }

    let result = try Driver.compileToTemporary(
      FileManager.default.temporaryFile(containing: "public fun main() {}"),
      withOptions: ["--emit", "intel-asm"])
    XCTAssert(result.status.isSuccess)
    result.checkDiagnosticText(is: "")
    XCTAssert(FileManager.default.fileExists(atPath: result.output.relativePath))
  }

  func testBinary() throws {
    if swiftyLLVMMandatoryPassesCrash { return }

    let result = try Driver.compileToTemporary(
      FileManager.default.temporaryFile(containing: "public fun main() {}"),
      withOptions: ["--emit", "binary"])
    XCTAssert(result.status.isSuccess)
    result.checkDiagnosticText(is: "")

    #if os(Windows)
      XCTAssert(FileManager.default.fileExists(atPath: result.output.relativePath + ".exe"))
    #else
      XCTAssert(FileManager.default.fileExists(atPath: result.output.relativePath))
    #endif
  }

  func testFreestanding() throws {
    let source = try FileManager.default.temporaryFile(
      containing: "public fun main() { print(0) }")
    let result = try Driver.compileToTemporary(source, withOptions: ["--freestanding"])
    XCTAssertFalse(result.status.isSuccess)
    result.checkDiagnosticText(
      is: """
        \(source.relativePath):1.21-26: error: undefined name 'print' in this scope
        public fun main() { print(0) }
                            ~~~~~

        """)
  }

  func testParseFailure() throws {
    let source = try FileManager.default.temporaryFile(containing: "fun x")
    let result = try Driver.compileToTemporary(source, withOptions: [])
    XCTAssertFalse(result.status.isSuccess)
    result.checkDiagnosticText(
      is: """
        \(source.relativePath):1.6: error: expected function signature
        fun x
             ^

        """)
  }

  func testTypeCheckSuccess() throws {
    let result = try Driver.compileToTemporary(
      FileManager.default.temporaryFile(containing: "public fun main() {}"),
      withOptions: ["--typecheck"])
    XCTAssert(result.status.isSuccess)
    result.checkDiagnosticText(is: "")
    XCTAssertFalse(FileManager.default.fileExists(atPath: result.output.relativePath))
  }

  func testTypeCheckFailure() throws {
    let source = try FileManager.default.temporaryFile(containing: "public fun main() { foo() }")
    let result = try Driver.compileToTemporary(source, withOptions: ["--typecheck"])
    XCTAssertFalse(result.status.isSuccess)
    result.checkDiagnosticText(
      is: """
        \(source.relativePath):1.21-24: error: undefined name 'foo' in this scope
        public fun main() { foo() }
                            ~~~

        """)
    XCTAssertFalse(FileManager.default.fileExists(atPath: result.output.relativePath))
  }

}

extension Driver.CompilationResult {

  /// `XCTAssert`'s that `diagnosticText` matches `expectedDiagnostics`, with better output
  /// formatting on failure.
  func checkDiagnosticText(
    `is` expectedDiagnostics: String, file: StaticString = #filePath, line: UInt = #line
  ) {
    let observedDiagnostics = diagnostics.rendered()
    XCTAssertEqual("\n" + observedDiagnostics, "\n" + expectedDiagnostics, file: file, line: line)
  }

}
