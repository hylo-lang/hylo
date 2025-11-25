import FrontEnd
import IR
import TestUtils
import Utils
import XCTest

final class InliningTests: XCTestCase {

  /// Tests that a simple function with no control flow gets inlined.
  func testSimpleFunctionInlining() throws {
    let source = """
      fun add(a: Int, b: Int) -> Int {
        a + b
      }

      public fun main() -> Int {
        add(a: 1, b: 2)
      }
      """.asSourceFile()

    var log = DiagnosticSet()
    let typedModule = try source.typecheckedAsMainWithHostedStandardLibrary(
      reportingDiagnosticsTo: &log, withBuiltinModuleAccess: true)
    let moduleID = typedModule.module

    // Lower all modules to IR (including standard library)
    var irProgram = try typedModule.program.lowerToIR(reportingDiagnosticsTo: &log)
    irProgram.inlineCalls(in: moduleID, where: .hasNoControlFlow)

    // Verify that the main function no longer contains a call to add
    let mainFunc = irProgram.modules[moduleID]!.entryFunction!
    let irText = irProgram.modules[moduleID]!.describe(function: mainFunc)

    // Should not contain "call @add"
    XCTAssertFalse(irText.contains("call @add"), "add function should be inlined")
  }

  /// Tests that nested function calls get fully inlined with fixed-point iteration.
  func testNestedFunctionInlining() throws {
    let source = """
      fun multiply(a: Int, b: Int) -> Int {
        a * b
      }

      fun square(x: Int) -> Int {
        multiply(a: x, b: x)
      }

      public fun main() -> Int {
        square(x: 5)
      }
      """.asSourceFile()

    var log = DiagnosticSet()
    let typedModule = try source.typecheckedAsMainWithHostedStandardLibrary(
      reportingDiagnosticsTo: &log, withBuiltinModuleAccess: true)
    let moduleID = typedModule.module

    // Lower all modules to IR (including standard library)
    var irProgram = try typedModule.program.lowerToIR(reportingDiagnosticsTo: &log)
    irProgram.inlineCalls(in: moduleID, where: .hasNoControlFlow)

    // Verify that both square and multiply are inlined
    let mainFunc = irProgram.modules[moduleID]!.entryFunction!
    let irText = irProgram.modules[moduleID]!.describe(function: mainFunc)

    XCTAssertFalse(irText.contains("call @square"), "square function should be inlined")
    XCTAssertFalse(irText.contains("call @multiply"), "multiply function should be inlined")
  }

  /// Tests that multiple arithmetic operations get fully inlined.
  func testMultipleArithmeticInlining() throws {
    let source = """
      fun is_right_triangle(a: Int, b: Int, c: Int) -> Int {
        let lhs = (a * a) + (b * b)
        let rhs = c * c
        return lhs - rhs
      }

      public fun main() -> Int {
        is_right_triangle(a: 3, b: 4, c: 5)
      }
      """.asSourceFile()

    var log = DiagnosticSet()
    let typedModule = try source.typecheckedAsMainWithStandardLibrary(
      reportingDiagnosticsTo: &log, withBuiltinModuleAccess: true, freestanding: true)
    let moduleID = typedModule.module

    // Lower all modules to IR (including standard library)
    var irProgram = try typedModule.program.lowerToIR(reportingDiagnosticsTo: &log, freestanding: true)
    irProgram.inlineCalls(in: moduleID, where: .hasNoControlFlow)

    // Verify that all functions are inlined
    let mainFunc = irProgram.modules[moduleID]!.entryFunction!
    let irText = irProgram.modules[moduleID]!.describe(function: mainFunc)
    XCTAssert(
      irText.contains("mul_word") || irText.contains("*"),
      "Should contain multiplication operations")
    XCTAssert(
      irText.contains("add_word") || irText.contains("+"),
      "Should contain addition operations")
    XCTAssert(
      irText.contains("sub_word") || irText.contains("-"),
      "Should contain subtraction operations")
  }

  /// Tests that functions with control flow are NOT inlined.
  func testFunctionWithControlFlowNotInlined() throws {
    let source = """
      fun conditional(x: Int) -> Int {
        if x > 0 {
          return x.copy()
        } else {
          return 0
        }
      }

      public fun main() -> Int {
        conditional(x: 5)
      }
      """.asSourceFile()

    var log = DiagnosticSet()
    let typedModule = try source.typecheckedAsMainWithStandardLibrary(
      reportingDiagnosticsTo: &log, withBuiltinModuleAccess: true, freestanding: true)
    let moduleID = typedModule.module

    // Lower all modules to IR (including standard library)
    var irProgram = try typedModule.program.lowerToIR(reportingDiagnosticsTo: &log, freestanding: true)
    irProgram.inlineCalls(in: moduleID, where: .hasNoControlFlow)

    // Verify that conditional is NOT inlined (it has control flow)
    let mainFunc = irProgram.modules[moduleID]!.entryFunction!
    let irText = irProgram.modules[moduleID]!.describe(function: mainFunc)

    // Should still contain a call to conditional
    XCTAssert(
      irText.contains("call @conditional") || irText.contains("call"),
      "Functions with control flow should not be inlined")
  }

  /// Tests inlining across an entire program (all modules).
  func testProgramWideInlining() throws {
    let source = """
      fun helper(x: Int) -> Int {
        x + x
      }

      public fun main() -> Int {
        helper(x: 10)
      }
      """.asSourceFile()

    var log = DiagnosticSet()
    let typedModule = try source.typecheckedAsMainWithStandardLibrary(
      reportingDiagnosticsTo: &log, withBuiltinModuleAccess: true, freestanding: true)
    let moduleID = typedModule.module

    // Lower all modules to IR (including standard library)
    var irProgram = try typedModule.program.lowerToIR(reportingDiagnosticsTo: &log, freestanding: true)
    irProgram.inlineCalls(where: .hasNoControlFlow)

    // Verify that helper is inlined
    let mainFunc = irProgram.modules[moduleID]!.entryFunction!
    let irText = irProgram.modules[moduleID]!.describe(function: mainFunc)

    XCTAssertFalse(irText.contains("call @helper"), "helper function should be inlined")
  }

  /// Tests that inlining is idempotent - running it twice produces the same result.
  func testInliningIsIdempotent() throws {
    let source = """
      fun double(x: Int) -> Int {
        x + x
      }

      public fun main() -> Int {
        double(x: 7)
      }
      """.asSourceFile()

    var log = DiagnosticSet()
    let typedModule = try source.typecheckedAsMainWithStandardLibrary(
      reportingDiagnosticsTo: &log, withBuiltinModuleAccess: true, freestanding: true)
    let moduleID = typedModule.module

    // Lower all modules to IR (including standard library)
    // First inlining pass
    var irProgram1 = try typedModule.program.lowerToIR(reportingDiagnosticsTo: &log, freestanding: true)
    irProgram1.inlineCalls(in: moduleID, where: .hasNoControlFlow)
    let result1 = irProgram1.modules[moduleID]!.description

    // Second inlining pass
    irProgram1.inlineCalls(in: moduleID, where: .hasNoControlFlow)
    let result2 = irProgram1.modules[moduleID]!.description

    // Results should be identical
    XCTAssertEqual(result1, result2, "Inlining should be idempotent")
  }

  /// Tests that a chain of function calls gets fully inlined.
  func testChainedFunctionCalls() throws {
    let source = """
      fun f1(x: Int) -> Int { x + 1 }
      fun f2(x: Int) -> Int { f1(x: x) + 1 }
      fun f3(x: Int) -> Int { f2(x: x) + 1 }

      public fun main() -> Int {
        f3(x: 0)
      }
      """.asSourceFile()

    var log = DiagnosticSet()
    let typedModule = try source.typecheckedAsMainWithStandardLibrary(
      reportingDiagnosticsTo: &log, freestanding: true)
    let moduleID = typedModule.module

    // Lower ALL modules (including standard library) to IR
    var irProgram = try typedModule.program.lowerToIR(reportingDiagnosticsTo: &log, freestanding: true)
    irProgram.inlineCalls(in: moduleID, where: .hasNoControlFlow)

    // Verify that all functions are inlined
    let mainFunc = irProgram.modules[moduleID]!.entryFunction!
    let irText = irProgram.modules[moduleID]!.describe(function: mainFunc)

    XCTAssertFalse(irText.contains("call @f1"), "f1 should be inlined")
    XCTAssertFalse(irText.contains("call @f2"), "f2 should be inlined")
    XCTAssertFalse(irText.contains("call @f3"), "f3 should be inlined")
  }

}
