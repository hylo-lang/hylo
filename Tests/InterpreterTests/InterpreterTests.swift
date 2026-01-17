import Driver
import Foundation
import FrontEnd
import IR
import XCTest
import Utils

@testable import Interpreter

final class InterpreterRunTests: XCTestCase {

  private func run(_ s: SourceFile) throws {
    let module = try s.loweredToIRAsMainWithHostedStandardLibrary()
    let program = IR.Program.init(syntax: module.program, modules: [module.id: module])
    var executor = Interpreter(program)
    while executor.isRunning {
      try executor.step()
    }
  }

  func testEmptyMain() throws {
    let p =
      """
        public fun main() { }
      """.asSourceFile();
    try p.runOnInterpreterAsMainWithHostedStandardLibrary()
  }

  func testLocalVariables() throws {
    let p =
      """
        public fun main() {
          let x = 2 as Int32
          let y = 4 as Int64
        }
      """.asSourceFile()
    try p.runOnInterpreterAsMainWithHostedStandardLibrary()
  }

}
