import Core
import FrontEnd
import XCTest

final class TypeCheckerTests: XCTestCase {

  func testTypeChecker() throws {
    try checkAnnotatedValFileDiagnostics(
      in: "TestCases/TypeChecking",
      { (source, diagnostics) in
        var ast = AST.coreModule
        let module = ast.insert(synthesized: ModuleDecl(source.baseName))

        _ = try Parser.parse(source, into: module, in: &ast, diagnostics: &diagnostics)

        // Run the type checker.
        _ = try TypedProgram(ast, diagnostics: &diagnostics)
      })
  }
}
