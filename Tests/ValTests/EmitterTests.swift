import XCTest
import Compiler
import Utils

final class EmitterTests: XCTestCase {

  func testEmitter() throws {
    // Locate the test cases.
    let testCaseDirectory = try XCTUnwrap(
      Bundle.module.url(forResource: "TestCases/Lowering", withExtension: nil),
      "No test cases")

    // Prepare an AST with the core module loaded.
    var baseAST = AST()
    baseAST.importCoreModule()

    // Execute the test cases.
    try TestCase.executeAll(in: testCaseDirectory, { (tc) in
      // Create an AST for the test case.
      var ast = baseAST

      // Create a module for the input.
      let module = try ast.insert(wellFormed: ModuleDecl(name: tc.name))

      // Parse the input.
      let (_, parseDiagnostics) = try Parser.parse(tc.source, into: module, in: &ast)
      if parseDiagnostics.contains(where: { $0.level == .error }) {
        XCTFail("\(tc.name): parsing failed")
        return
      }

      // Run the type checker.
      var checker = TypeChecker(program: ScopedProgram(ast: ast))
      if !checker.check(module: module) {
        XCTFail("\(tc.name): type checking failed")
        return
      }

      let typedProgram = TypedProgram(
        annotating: checker.program,
        declTypes: checker.declTypes,
        exprTypes: checker.exprTypes,
        implicitCaptures: checker.implicitCaptures,
        referredDecls: checker.referredDecls,
        foldedSequenceExprs: checker.foldedSequenceExprs)

      // Emit Val's IR.
      var emitter = Emitter(program: typedProgram)
      var irModule = emitter.emit(module: module)

      // Run mandatory IR analysis and transformation passes.
      var pipeline: [TransformPass] = [
        ImplicitReturnInsertionPass(),
        DefiniteInitializationPass(program: typedProgram),
        LifetimePass(program: typedProgram),
      ]

      var success = true
      var diagnostics: [Diagnostic] = []

      for i in 0 ..< pipeline.count {
        for f in 0 ..< irModule.functions.count {
          success = pipeline[i].run(function: f, module: &irModule) && success
          diagnostics.append(contentsOf: pipeline[i].diagnostics)
        }
        if !success { break }
      }

      // Create a diagnostic checker.
      var diagnosticChecker = DiagnosticChecker(testCaseName: tc.name, diagnostics: diagnostics)

      // Process the test annotations.
      for annotation in tc.annotations {
        switch annotation.command {
        case "expect-failure":
          XCTAssert(!success, "\(tc.name): lowering succeeded, but expected failure")
        case "expect-success":
          XCTAssert(success, "\(tc.name): lowering failed, but expected success")
        case "diagnostic":
          diagnosticChecker.handle(annotation)
        default:
          XCTFail("\(tc.name): unexpected test command: '\(annotation.command)'")
        }
      }
    })
  }

}
