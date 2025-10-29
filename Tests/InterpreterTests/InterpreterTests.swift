import Driver
import Foundation
import IR
import Testing
import Utils

@testable import Interpreter

@Suite struct InterpreterRunTests {

  @Test func emptyMain() throws {
    let input =
      """
        public fun main() { }
      """.asSourceFile()
    let module = try input.loweredToIRAsMainWithHostedStandardLibrary();
    let program = IR.Program.init(syntax: module.program, modules: [module.id: module]);
    var executor = Interpreter(program);
    while executor.isRunning {
      try executor.step()
    }
  }

}
