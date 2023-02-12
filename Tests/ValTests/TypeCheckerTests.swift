import Core
import FrontEnd
import TestUtils
import XCTest

final class TypeCheckerTests: XCTestCase {

  func testTypeChecker() throws {
    try checkAnnotatedValFileDiagnostics(
      in: Bundle.module.url(forResource: "TestCases/TypeChecking", withExtension: nil)!,
      { (source, diagnostics) in
        var ast = AST.coreModule
        _ = try ast.makeModule(
          source.baseName, sourceCode: [source], builtinModuleAccess: true,
          diagnostics: &diagnostics)
        _ = try TypedProgram(ast, diagnostics: &diagnostics)
      })
  }

}
