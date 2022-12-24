import FrontEnd
import Utils
import XCTest
import Core

final class TypeCheckerTests: XCTestCase {

  func testTypeChecker() throws {
    // Locate the test cases.
    let testCaseDirectory = try XCTUnwrap(
      Bundle.module.url(forResource: "TestCases/TypeChecking", withExtension: nil),
      "No test cases")

    // Prepare an AST with the core module loaded.
    var baseAST = AST()
    baseAST.importCoreModule()

    // Execute the test cases.
    try withFiles(in: testCaseDirectory, { (url) in
      // Parse the test case.
      let tc = try TestCase(source: SourceFile(contentsOf: url))

      // Create a module for the input.
      var ast = baseAST
      let module = try ast.insert(wellFormed: ModuleDecl(name: tc.name))

      // Parse the input.
      let parseResult = try Parser.parse(tc.source, into: module, in: &ast)
      var parseErrorOccured = false
      for d in parseResult.diagnostics where d.level == .error {
        record(XCTIssue(d))
        parseErrorOccured = true
      }

      // Exit if a parse error occured.
      if parseErrorOccured { return true }

      // Run the type checker.
      var checker = TypeChecker(program: ScopedProgram(ast: ast))
      let success = checker.check(module: module)

      // Execute the test annotations.
      var handler = TestAnnotationHandler(
        testCaseRanToCompletion: success, diagnostics: Array(checker.diagnostics))
      handler.handle(tc.annotations)
      for issue in handler.finalize() {
        record(issue)
      }

      // Move to the next test case.
      return true
    })
  }

}
