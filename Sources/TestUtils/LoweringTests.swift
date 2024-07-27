import FrontEnd
import IR
import StandardLibrary
import Utils
import XCTest

extension XCTestCase {

  /// Lowers the hylo file at `hyloFilePath` to IR, applying any mandatory passes, and `XCTAssert`s
  /// that diagnostics and thrown errors match annotated expectations.
  @nonobjc
  public func lowerToFinishedIR(_ hyloFilePath: String, expectSuccess: Bool) throws {

    try checkAnnotatedHyloFileDiagnostics(inFileAt: hyloFilePath, expectSuccess: expectSuccess) {
      (valSource, diagnostics) in
      // Note: built-in module is visible so that we can test built-in function calls.
      var ast = try Host.freestandingLibraryAST.get()

      let module = try ast.loadModule(
        valSource.baseName, sourceCode: [valSource], builtinModuleAccess: true,
        reportingDiagnosticsTo: &diagnostics)

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
