import ArgumentParser
import Utils
import ValCommand
import XCTest

final class ValCommandTests: XCTestCase {

  func testNoInput() throws {
    XCTAssertThrowsError(try ValCommand.parse([]))
  }

  func testRawAST() throws {
    let input = try url(forSourceNamed: "Success")
    let result = try compile(input, with: ["--emit", "raw-ast"])
    result.assertSuccess()
  }

  func testRawIR() throws {
    let input = try url(forSourceNamed: "Success")
    let result = try compile(input, with: ["--emit", "raw-ir"])
    result.assertSuccess()
  }

  func testIR() throws {
    let input = try url(forSourceNamed: "Success")
    let result = try compile(input, with: ["--emit", "ir"])
    result.assertSuccess()
  }

  func testCPP() throws {
    let input = try url(forSourceNamed: "Success")
    let result = try compile(input, with: ["--emit", "cpp"])
    result.assertSuccess(checkOutput: false)

    let baseURL = result.output.deletingPathExtension()
    XCTAssert(
      FileManager.default.fileExists(atPath: baseURL.appendingPathExtension("h").relativePath))
    XCTAssert(
      FileManager.default.fileExists(atPath: baseURL.appendingPathExtension("cpp").relativePath))
  }

  func testBinary() throws {
    let input = try XCTUnwrap(
      Bundle.module.url(forResource: "Success", withExtension: ".val", subdirectory: "Inputs"),
      "No inputs")

    let result = try compile(input, with: ["--emit", "binary"])
    XCTAssert(result.status.isSuccess)
    XCTAssert(result.stderr.isEmpty)
    XCTAssert(FileManager.default.fileExists(atPath: result.output.relativePath))
  }

  func testParseFailure() throws {
    let input = try url(forFileContaining: "fun x")
    let result = try compile(input, with: [])
    XCTAssertFalse(result.status.isSuccess)
    XCTAssertEqual(
      result.stderr,
      """
      \(input.relativePath):1:6: error: expected function signature
      fun x
           ^

      """)
  }

  func testTypeCheckSuccess() throws {
    let input = try url(forSourceNamed: "Success")
    let result = try compile(input, with: ["--typecheck"])
    result.assertSuccess(checkOutput: false)
  }

  func testTypeCheckFailure() throws {
    let input = try url(forFileContaining: "public fun main() { foo() }")
    let result = try compile(input, with: ["--typecheck"])
    XCTAssertFalse(result.status.isSuccess)
    XCTAssertEqual(
      result.stderr,
      """
      \(input.relativePath):1:21: error: undefined name 'foo' in this scope
      public fun main() { foo() }
                          ~~~

      """)
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

  /// Writes `s` to a temporary file and returns its URL.
  private func url(forFileContaining s: String) throws -> URL {
    let f = FileManager.default.temporaryFile()
    try s.write(to: f, atomically: true, encoding: .utf8)
    return f
  }

}
