import Core
import FrontEnd
import IR
import Utils
import XCTest

final class EmitterTests: XCTestCase {

  func testEmitter() throws {
    try checkAnnotatedValFileDiagnostics(
      in: "TestCases/Lowering",
      { (source, diagnostics) in
        // Create a module for the input.
        var ast = AST.coreModule
        let module = try ast.makeModule(
          source.baseName, sourceCode: [source], diagnostics: &diagnostics)

        // Run the type checker
        // Note: built-in module is visible so that we can test built-in function calls.
        var checker = TypeChecker(program: ScopedProgram(ast), isBuiltinModuleVisible: true)
        checker.check(module: module)
        diagnostics.formUnion(checker.diagnostics)
        try diagnostics.throwOnError()

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
            diagnostics.formUnion(pipeline[i].diagnostics)
          }
          try diagnostics.throwOnError()
        }
      })
  }

}
