import Core
import FrontEnd
import XCTest

final class TypeCheckerTests: XCTestCase, ValTestRunner {

  static var testCaseDirectoryPath: String { "TestCases/TypeChecking" }

  func testTypeChecker() throws {
    // Prepare an AST with the core module loaded.
    var baseAST = AST()
    baseAST.importCoreModule()

    try runValTests({ (name, source) in
      // Create a module for the input.
      var ast = baseAST
      let module = try! ast.insert(wellFormed: ModuleDecl(name: name))

      // Parse the input.
      let parseResult = Parser.parse(source, into: module, in: &ast)
      if parseResult.failed {
        return .init(ranToCompletion: false, diagnostics: parseResult.diagnostics)
      }

      // Run the type checker.
      var checker = TypeChecker(program: ScopedProgram(ast: ast))
      let success = checker.check(module: module)
      return .init(
        ranToCompletion: success,
        diagnostics: parseResult.diagnostics + Array(checker.diagnostics))
    })
  }

}
