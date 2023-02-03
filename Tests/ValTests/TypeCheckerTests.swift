import Core
import FrontEnd
import XCTest

final class TypeCheckerTests: XCTestCase {

  func testTypeChecker() throws {
    // Prepare an AST with the core module loaded.
    var baseAST = AST()
    baseAST.importCoreModule()

    try checkAnnotatedValFileDiagnostics(
      in: "TestCases/TypeChecking",
      { (source, diagnostics) in
        var ast = baseAST
        let module = ast.insert(synthesized: ModuleDecl(name: source.baseName))

        _ = try Parser.parse(source, into: module, in: &ast, diagnostics: &diagnostics)

        // Run the type checker.
        var checker = TypeChecker(program: ScopedProgram(ast))
        checker.check(module: module)
        diagnostics.report(checker.diagnostics)
        try diagnostics.throwOnError()
      })
  }
}
