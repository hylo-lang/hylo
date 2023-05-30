import Core
import FrontEnd
import IR
import Utils
import XCTest

extension XCTestCase {

  /// Lowers the val file at `valFilePath` to IR, applying any mandatory passes, and `XCTAssert`s
  /// that diagnostics and thrown errors match annotated expectations.
  func lowerToFinishedIR(_ valFilePath: String, expectSuccess: Bool) throws {

    try checkAnnotatedValFileDiagnostics(inFileAt: valFilePath, expectSuccess: expectSuccess) {
      (valSource, diagnostics) in
      // Note: built-in module is visible so that we can test built-in function calls.
      var ast = AST.coreModule
      let module = try ast.makeModule(
        valSource.baseName, sourceCode: [valSource], builtinModuleAccess: true,
        diagnostics: &diagnostics)

      // Run the type checker
      let typedProgram = try TypedProgram(ast, diagnostics: &diagnostics)

      // Emit Val's IR.
      var irModule = try Module(lowering: module, in: typedProgram, diagnostics: &diagnostics)

      // Run mandatory IR analysis and transformation passes.
      try irModule.applyMandatoryPasses(reportingDiagnosticsInto: &diagnostics)

    }

  }

}
