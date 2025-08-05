import Utils
import XCTest
import Interpreter
import IR
import Driver

final class InterpreterTests: XCTestCase {

  func testInterpreter() throws {
    let source = try FileManager.default.temporaryFile(
      containing: #"public fun main() { print("Hello, World!") }"#)

    let compilation = try Driver.compileToTemporary(
      source, withOptions: ["--last-phase=lowering"])
    try compilation.diagnostics.throwOnError()

    func runAndCheckOutput(_ ir: IR.Program) throws {
      var executor = Interpreter(ir)
      while executor.isRunning { try executor.step() }
      let output = executor.standardOutput

      // Remember, Windows has a different newline character
      XCTAssert(output.last?.isNewline ?? false, "Expected a final newline")
      XCTAssertEqual(output.dropLast(), "Hello, World!")
    }

    XCTAssertNoThrow(try runAndCheckOutput(compilation.ir!))
  }

}
