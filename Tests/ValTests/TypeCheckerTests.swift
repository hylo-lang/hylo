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
        // Create a module for the input.
        var ast = baseAST
        let module = try! ast.insert(wellFormed: ModuleDecl(name: source.baseName))

        // Parse the input.
        let parseResult = Parser.parse(source, into: module, in: &ast)
        diagnostics.report(parseResult.diagnostics)
        if parseResult.failed {
          throw DiagnosedError(diagnostics)
        }

        // Run the type checker.
        var checker = TypeChecker(program: ScopedProgram(ast: ast))
        diagnostics.report(checker.diagnostics)
        let wellTyped = checker.check(module: module)
        diagnostics.report(checker.diagnostics)
        if !wellTyped {
          throw DiagnosedError(diagnostics)
        }
      })
  }
}
