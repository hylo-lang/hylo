import FrontEnd
import StandardLibrary
import Utils
import XCTest

extension XCTestCase {

  /// Type-checks the Hylo file at `hyloFilePath`, `XCTAssert`ing that diagnostics and thrown
  /// errors match annotated expectations.
  @nonobjc
  public func typeCheck(_ hyloFilePath: String, expectSuccess: Bool) throws {

    try checkAnnotatedHyloFileDiagnostics(inFileAt: hyloFilePath, expectSuccess: expectSuccess) {
      (source, diagnostics) in

      var ast = try Host.freestandingLibraryAST.get()

      _ = try ast.makeModule(
        source.baseName, sourceCode: [source], builtinModuleAccess: true,
        diagnostics: &diagnostics)
      let base = ScopedProgram(ast)
      _ = try TypedProgram(annotating: base, reportingDiagnosticsTo: &diagnostics)
    }

  }

}
