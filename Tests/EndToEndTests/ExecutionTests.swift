import TestUtils
import XCTest

final class ExecutionTests: XCTestCase {

  func testHelloWorld() throws {
    if swiftyLLVMMandatoryPassesCrash { return }

    let f = FileManager.default.makeTemporaryFileURL()
    let s = #"public fun main() { print("Hello, World!") }"#
    try s.write(to: f, atomically: true, encoding: .utf8)

    let executable = try compile(f, with: ["--emit", "binary", "-o", "hello"])

    func runAndCheckOutput() throws {
      let outputText = try Process.run(executable).standardOutput[]

      // Remember, Windows has a different newline character
      XCTAssert(outputText.last?.isNewline ?? false, "Expected a final newline")
      XCTAssertEqual(outputText.dropLast(), "Hello, World!")
    }

    XCTAssertNoThrow(try runAndCheckOutput())
  }

}
