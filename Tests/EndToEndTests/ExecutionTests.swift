import TestUtils
import XCTest

final class ExecutionTests: XCTestCase {

  func testHelloWorld() throws {
    let f = FileManager.default.makeTemporaryFileURL()
    let s = #"public fun main() { print("Hello, World!") }"#
    try s.write(to: f, atomically: true, encoding: .utf8)

    let output = try compile(f, with: ["--emit", "binary", "-o", "hello"])
    let result = try run(output)
    XCTAssertEqual(result.status, 0, "Exit code is \(result.status)")
    XCTAssert(result.standardOutput.last?.isNewline ?? false)  // Allow for windows newline.
    XCTAssertEqual(result.standardOutput.dropLast(), "Hello, World!")
  }

}
