import Driver
import Foundation
import IR
import Testing
import Utils

@testable import Interpreter

@Suite struct RunInterpreter {

  func run(_ program: String) throws -> Interpreter {
    let source = try FileManager.default.temporaryFile(containing: program)

    let compilation = try Driver.compileToTemporary(
      source, withOptions: ["--last-phase=lowering"])
    try compilation.diagnostics.throwOnError()

    var executor = Interpreter(compilation.ir!)

    #expect(throws: Never.self) {
      while executor.isRunning { try executor.step() }
    }

    return executor
  }

  @Test func emptyProgramShouldWork() throws {
    _ = try run(#"public fun main() {  }"#)
  }

}
