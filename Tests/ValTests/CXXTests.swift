import CodeGenCXX
import Core
import FrontEnd
import Utils
import XCTest

final class CXXTests: XCTestCase, ValTestRunner {

  static var testCaseDirectoryPath: String { "TestCases/CXX" }

  func testTranspiler() throws {
    // Prepare an AST with the core module loaded.
    var baseAST = AST()
    baseAST.importCoreModule()

    try runValTests(
      { (name, source) -> CXXAnnotationHandler in
        // Create a module for the input.
        var ast = baseAST
        let module = try! ast.insert(wellFormed: ModuleDecl(name: name))

        // Parse the input.
        let parseResult = Parser.parse(source, into: module, in: &ast)
        var diagnostics = parseResult.diagnostics
        if parseResult.failed {
          return .init(.init(module: nil, ranToCompletion: false, diagnostics: diagnostics))
        }

        // Run the type checker.
        var checker = TypeChecker(program: ScopedProgram(ast: ast))
        diagnostics.append(contentsOf: checker.diagnostics)
        if !checker.check(module: module) {
          return .init(.init(module: nil, ranToCompletion: false, diagnostics: diagnostics))
        }

        let typedProgram = TypedProgram(
          annotating: checker.program,
          declTypes: checker.declTypes,
          exprTypes: checker.exprTypes,
          implicitCaptures: checker.implicitCaptures,
          referredDecls: checker.referredDecls,
          foldedSequenceExprs: checker.foldedSequenceExprs)

        // TODO: Run IR transform passes

        // Transpile the module.
        var transpiler = CXXTranspiler(program: typedProgram)
        let cxxModule = transpiler.emit(module: typedProgram[module])

        return .init(.init(module: cxxModule, ranToCompletion: true, diagnostics: diagnostics))
      })
  }

}
