import ArgumentParser
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

  func testTypeCheckSuccess() throws {
    let input = try url(forSourceNamed: "Success")
    let result = try compile(input, with: ["--typecheck"])
    result.assertSuccess(checkOutput: false)
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

}
