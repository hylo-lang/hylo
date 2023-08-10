import Core
import FrontEnd
import TestUtils
import XCTest

extension XCTestCase {

  /// Type-checks the val file at `valFilePath`, `XCTAssert`ing that diagnostics and thrown
  /// errors match annotated expectations.
  func typeCheck(_ valFilePath: String, expectSuccess: Bool) throws {

    try checkAnnotatedValFileDiagnostics(inFileAt: valFilePath, expectSuccess: expectSuccess) {
      (source, diagnostics) in

      var ast = AST.coreModule
      _ = try ast.makeModule(
        source.baseName, sourceCode: [source], builtinModuleAccess: true,
        diagnostics: &diagnostics)
      let base = ScopedProgram(ast)
      _ = try TypedProgram(annotating: base, reportingDiagnosticsTo: &diagnostics)
    }

  }

}
