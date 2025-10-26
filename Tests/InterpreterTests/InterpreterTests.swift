import Driver
import Foundation
import IR
import Testing
import Utils

@testable import Interpreter

@Suite struct InterpreterRunTests {

  func executor(for program: String) throws -> Interpreter {
    let source = try FileManager.default.temporaryFile(containing: program)

    let compilation = try Driver.compileToTemporary(
      source, withOptions: ["--last-phase=lowering"])
    try compilation.diagnostics.throwOnError()

    return Interpreter(compilation.ir!)
  }

  @Test func emptyProgramShouldWork() throws {
    var executor = try executor(for: #"public fun main() {  }"#)
    #expect(throws: Never.self) {
      while executor.isRunning { try executor.step() }
    }
  }

}
