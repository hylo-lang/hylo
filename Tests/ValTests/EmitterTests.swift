import Core
import FrontEnd
import IR
import Utils
import XCTest

final class EmitterTests: XCTestCase, ValTestRunner {

  static var testCaseDirectoryPath: String { "TestCases/Lowering" }

  func testEmitter() throws {
    // Prepare an AST with the core module loaded.
    var baseAST = AST()
    baseAST.importCoreModule()

    try runValTests(
      { (name, source) -> DefaultTestAnnotationHandler in
        // Create a module for the input.
        var ast = baseAST
        let module = try! ast.insert(wellFormed: ModuleDecl(name: name))

        // Parse the input.
        let parseResult = Parser.parse(source, into: module, in: &ast)
        var diagnostics = parseResult.diagnostics
        if parseResult.failed {
          return .init(ranToCompletion: false, diagnostics: diagnostics)
        }

        // Run the type checker.
        var checker = TypeChecker(program: ScopedProgram(ast: ast))
        diagnostics.append(contentsOf: checker.diagnostics)
        if !checker.check(module: module) {
          return .init(ranToCompletion: false, diagnostics: diagnostics)
        }

        let typedProgram = TypedProgram(
          annotating: checker.program,
          declTypes: checker.declTypes,
          exprTypes: checker.exprTypes,
          implicitCaptures: checker.implicitCaptures,
          referredDecls: checker.referredDecls,
          foldedSequenceExprs: checker.foldedSequenceExprs)

        // Emit Val's IR.
        var irModule = Module(module, in: typedProgram)

        // Run mandatory IR analysis and transformation passes.
        var pipeline: [TransformPass] = [
          ImplicitReturnInsertionPass(),
          DefiniteInitializationPass(program: typedProgram),
          LifetimePass(program: typedProgram),
        ]

        var success = true
        for i in 0..<pipeline.count {
          for f in 0..<irModule.functions.count {
            success = pipeline[i].run(function: f, module: &irModule) && success
            diagnostics.append(contentsOf: pipeline[i].diagnostics)
          }
          if !success { break }
        }

        return .init(ranToCompletion: success, diagnostics: diagnostics)
      })
  }

}
