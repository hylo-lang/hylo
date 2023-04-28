import Core
import FrontEnd
import IR
import Utils
import XCTest

final class LoweringTests: XCTestCase {

  func testLowering() throws {
    try checkAnnotatedValFileDiagnostics(inSuiteAt: "TestCases/Lowering") { (source, log) in
      var ir = try lower(source, log: &log)
      try ir.applyMandatoryPasses(reportingDiagnosticsInto: &log)
    }
  }

  /// Returns the Val IR of `source`, reporting diagnostics to `log`.
  private func lower(_ source: SourceFile, log: inout DiagnosticSet) throws -> IR.Module {
    // Note: built-in module is visible so that we can test built-in function calls.
    var ast = AST.coreModule
    let module = try ast.makeModule(
      source.baseName, sourceCode: [source],
      builtinModuleAccess: true, diagnostics: &log)

    // Run the type checker
    let typedProgram = try TypedProgram(ast, diagnostics: &log)

    // Emit Val's IR.
    return try Module(lowering: module, in: typedProgram, diagnostics: &log)
  }

}
