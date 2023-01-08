import Core
import FrontEnd
import IR
import Utils
import XCTest

final class EmitterTests: XCTestCase {

  func testEmitter() throws {
    // Prepare an AST with the core module loaded.
    var baseAST = AST()
    baseAST.importCoreModule()

    try checkAnnotatedValFileDiagnostics(
      in: "TestCases/Lowering",
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
        let wellTyped = checker.check(module: module)
        diagnostics.report(checker.diagnostics)
        if !wellTyped {
          throw DiagnosedError(diagnostics)
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
        for i in 0 ..< pipeline.count {
          for f in 0 ..< irModule.functions.count {
            success = pipeline[i].run(function: f, module: &irModule) && success
            diagnostics.report(pipeline[i].diagnostics)
          }
          if !success { throw DiagnosedError(diagnostics) }
        }
      })
  }

}
