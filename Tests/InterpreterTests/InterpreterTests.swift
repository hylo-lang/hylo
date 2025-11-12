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

  func testStackAllocAndDealloc() throws {
    let input =
      """
        public fun main() {
          let x = 2
          let y = 2
        }
      """.asSourceFile()
    let module = try input.loweredToIRAsMainWithHostedStandardLibrary();
    let program = IR.Program.init(syntax: module.program, modules: [module.id: module]);
    var executor = Interpreter(program);
    while executor.isRunning {
      try executor.step()
    }
  }

  func testFunctionCall() throws {
    let input =
      """
       public fun dummy() {}

       public fun id(_ x: sink Int) -> Int {
         dummy()
         return x
       }

       public fun main() {
         let x = 2
         let y = id(x)
         let z = 3
       }
      """.asSourceFile()
    let module = try input.loweredToIRAsMainWithHostedStandardLibrary();
    let program = IR.Program.init(syntax: module.program, modules: [module.id: module]);
    var executor = Interpreter(program);
    while executor.isRunning {
      try executor.step()
    }
  }

  func testBranch() throws {
    let input =
      """
       public fun select(_ cond: Bool, _ first: sink Int, _ second: sink Int) -> Int {
         if cond {
           return first
         }else{
           return second
         }
       }

       public fun main() {
         _ = select(true, 2, 3)
         _ = select(false, 2, 3)
       }
      """.asSourceFile()
    let module = try input.loweredToIRAsMainWithHostedStandardLibrary();
    let program = IR.Program.init(syntax: module.program, modules: [module.id: module]);
    var executor = Interpreter(program);
    while executor.isRunning {
      try executor.step()
    }
  }

}
