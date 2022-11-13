import XCTest
import Compiler

func check(_ haystack: String, contains needle: String.SubSequence) {
  XCTAssert(
    haystack.contains(needle),
    """

    \(String(reflecting: needle))
    not found in source
    \"""
    \(haystack)
    \"""
    """)
}

final class CXXTests: XCTestCase {

  func testTranspiler() throws {
    // Locate the test cases.
    let testCaseDirectory = try XCTUnwrap(
      Bundle.module.url(forResource: "TestCases/CXX", withExtension: nil),
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
        referredDecls: checker.referredDecls)

      // TODO: Run IR transform passes

      // Transpile the module.
      var transpiler = CXXTranspiler(program: typedProgram)
      let cxxModule = transpiler.emit(module: module)
      let cxxHeader = cxxModule.emitHeader()
      let cxxSource = cxxModule.emitSource()

      // Process the test annotations.
      for annotation in tc.annotations {
        switch annotation.command {
        case "cpp":
          let specifications = annotation.argument?.split(separator: "\n", maxSplits: 1) ?? []
          assert((1...2).contains(specifications.count))
          if specifications.count > 1 {
            check(cxxHeader, contains: specifications.first!)
          }
          check(cxxSource, contains: specifications.last!)

        default:
          XCTFail("\(tc.name): unexpected test command: '\(annotation.command)'")
        }
      }
    })
  }


}
