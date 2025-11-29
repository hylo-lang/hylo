import Driver
import Foundation
import IR
import XCTest
import Utils

@testable import Interpreter

final class InterpreterRunTests: XCTestCase {

  func testEmptyMain() throws {
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
