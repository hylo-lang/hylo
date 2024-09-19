import Driver
import TestUtils
import XCTest

final class ExecutionTests: XCTestCase {

  func testHelloWorld() throws {
    if swiftyLLVMMandatoryPassesCrash { return }

    let source = try FileManager.default.temporaryFile(
      containing: #"public fun main() { print("Hello, World!") }"#)

    let compilation = try Driver.compileToTemporary(
      source, withOptions: ["--emit", "binary", "-o", "hello"])
    try compilation.diagnostics.throwOnError()

    func runAndCheckOutput() throws {
      let output = try Process.run(compilation.output).standardOutput[]

      // Remember, Windows has a different newline character
      XCTAssert(output.last?.isNewline ?? false, "Expected a final newline")
      XCTAssertEqual(output.dropLast(), "Hello, World!")
    }

    XCTAssertNoThrow(try runAndCheckOutput())
  }

}
