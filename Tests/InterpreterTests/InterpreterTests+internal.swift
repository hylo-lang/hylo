import IR
import Utils
import XCTest

@testable import Interpreter

final class InterpreterInternalTests: XCTestCase {

  func testAsBoolForConstantOperand() throws {
    let p =
      """
        public fun main() { }
      """.asSourceFile()
    let module = try p.loweredToIRAsMainWithHostedStandardLibrary(withBuiltinModuleAccess: true)
    let program = IR.Program.init(syntax: module.program, modules: [module.id: module])
    let e = Interpreter(program)
    XCTAssertEqual(e.asBool(.constant(IntegerConstant(1))), true)
    XCTAssertEqual(e.asBool(.constant(IntegerConstant(0))), false)
  }

}
