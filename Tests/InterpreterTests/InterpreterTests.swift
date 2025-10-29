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
    var executor = Interpreter(program, withStackMemoryOfBytes: 512 * 1024);
    #expect(throws: Never.self) {
      while executor.isRunning { try executor.step() }
    }
  }

  @Test func stackAllocAndDealloc() throws {
    let input =
      """
        public fun main() {
          let x = 2
          let y = 2
        }
      """.asSourceFile()
    let module = try input.loweredToIRAsMainWithHostedStandardLibrary();
    let program = IR.Program.init(syntax: module.program, modules: [module.id: module]);
    var executor = Interpreter(program, withStackMemoryOfBytes: 512 * 1024);
    #expect(throws: Never.self) {
      while executor.isRunning { try executor.step() }
    }
  }

  @Test func functionCall() throws {
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
    var executor = Interpreter(program, withStackMemoryOfBytes: 512 * 1024);
    #expect(throws: Never.self) {
      while executor.isRunning { try executor.step() }
    }
  }

  @Test func branch() throws {
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
    var executor = Interpreter(program, withStackMemoryOfBytes: 512 * 1024);
    #expect(throws: Never.self) {
      while executor.isRunning { try executor.step() }
    }
  }

}
