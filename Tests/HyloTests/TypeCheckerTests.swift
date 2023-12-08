import Core
import FrontEnd
import StandardLibrary
import TestUtils
import Utils
import XCTest

extension XCTestCase {

  /// Type-checks the Hylo file at `hyloFilePath`, `XCTAssert`ing that diagnostics and thrown
  /// errors match annotated expectations.
  func typeCheck(_ hyloFilePath: String, expectSuccess: Bool) throws {

    try checkAnnotatedHyloFileDiagnostics(inFileAt: hyloFilePath, expectSuccess: expectSuccess) {
      (source, diagnostics) in

      var ast = Host.freestandingLibraryAST

      _ = try ast.makeModule(
        source.baseName, sourceCode: [source], builtinModuleAccess: true,
        diagnostics: &diagnostics)
      let base = ScopedProgram(ast)
      _ = try TypedProgram(annotating: base, reportingDiagnosticsTo: &diagnostics)
    }

  }

}
