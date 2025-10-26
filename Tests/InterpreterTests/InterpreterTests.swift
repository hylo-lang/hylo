import Driver
import Foundation
import IR
import Interpreter
import Utils
import XCTest

final class InterpreterTests: XCTestCase {

  func run(_ program: String) throws -> Interpreter {
    let source = try FileManager.default.temporaryFile(containing: program)

    let compilation = try Driver.compileToTemporary(
      source, withOptions: ["--last-phase=lowering"])
    try compilation.diagnostics.throwOnError()

    var executor = Interpreter(compilation.ir!)

    func runProgram() throws {
      while executor.isRunning { try executor.step() }
    }

    XCTAssertNoThrow(try runProgram())

    return executor
  }

  func testEmptyProgram() throws {
    _ = try run(#"public fun main() {  }"#)
  }

}
