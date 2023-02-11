import Core
import FrontEnd
import XCTest

final class TypeCheckerTests: XCTestCase {

  func testTypeChecker() throws {
    try checkAnnotatedValFileDiagnostics(
      in: "TestCases/TypeChecking",
      { (source, diagnostics) in
        var ast = AST.coreModule
        _ = try ast.makeModule(source.baseName, sourceCode: [source], diagnostics: &diagnostics)
        _ = try TypedProgram(ast, diagnostics: &diagnostics)
      })
  }

}
