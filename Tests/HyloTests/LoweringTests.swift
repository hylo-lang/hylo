import Core
import FrontEnd
import IR
import Utils
import XCTest
import StandardLibraryModules

extension XCTestCase {

  /// Lowers the hylo file at `hyloFilePath` to IR, applying any mandatory passes, and `XCTAssert`s
  /// that diagnostics and thrown errors match annotated expectations.
  func lowerToFinishedIR(_ hyloFilePath: String, expectSuccess: Bool) throws {

    try checkAnnotatedHyloFileDiagnostics(inFileAt: hyloFilePath, expectSuccess: expectSuccess) {
      (valSource, diagnostics) in
      // Note: built-in module is visible so that we can test built-in function calls.
      var ast = try coreLibraryModule[]
      let module = try ast.makeModule(
        valSource.baseName, sourceCode: [valSource], builtinModuleAccess: true,
        diagnostics: &diagnostics)

      // Run the type checker
      let base = ScopedProgram(ast)
      let typedProgram = try TypedProgram(annotating: base, reportingDiagnosticsTo: &diagnostics)

      // Emit Hylo IR.
      var irModule = try Module(
        lowering: module, in: typedProgram, reportingDiagnosticsTo: &diagnostics)

      // Run mandatory IR analysis and transformation passes.
      try irModule.applyMandatoryPasses(reportingDiagnosticsTo: &diagnostics)
    }

  }

}
