import Compiler
import Utils
import XCTest

final class TypeCheckerTests: XCTestCase {

  func testTypeChecker() throws {
    // Locate the test cases.
    let testCaseDirectory = try XCTUnwrap(
      Bundle.module.url(forResource: "TestCases/TypeChecking", withExtension: nil), "No test cases")

    // Prepare an AST with the core module loaded.
    var baseAST = AST()
    baseAST.importCoreModule()

    // Execute the test cases.
    try TestCase.executeAll(
      in: testCaseDirectory,
      { (tc) in
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
        let success = checker.check(module: module)

        // Create a diagnostic checker.
        var diagnosticChecker = DiagnosticChecker(
          testCaseName: tc.name, diagnostics: checker.diagnostics)

        // Process the test annotations.
        for annotation in tc.annotations {
          switch annotation.command {
          case "expect-failure":
            XCTAssert(!success, "\(tc.name): type checking succeeded, but expected failure")
          case "expect-success":
            XCTAssert(success, "\(tc.name): type checking failed, but expected success")
          case "diagnostic": diagnosticChecker.handle(annotation)
          default: XCTFail("\(tc.name): unexpected test command: '\(annotation.command)'")
          }
        }

        diagnosticChecker.finalize()
      })
  }

}
