import XCTest
import Compiler

func check(_ haystack: String, contains needle: String.SubSequence, for testFile: String) {
  XCTAssert(
    haystack.contains(needle),
    """


    Test file: \(testFile)
    ===========\(String(repeating: "=", count: testFile.count))

    \(String(reflecting: needle))
    not found in source
    \"""
    \(haystack)
    \"""
    """)
}

/// Remove trailing newlines from the given string subsequence
func rtrim(_ s: String.SubSequence) -> String.SubSequence {
  var str = s
  while !str.isEmpty && str.last! == "\n" {
    str = str.prefix(str.count-1)
  }
  return str
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
        implicitCaptures: checker.implicitCaptures,
        referredDecls: checker.referredDecls,
        foldedSequenceExprs: checker.foldedSequenceExprs)

      // TODO: Run IR transform passes

      // Transpile the module.
      var transpiler = CXXTranspiler(program: typedProgram)
      let cxxModule = transpiler.emit(module: module)
      let cxxHeader = cxxModule.emitHeader()
      let cxxSource = cxxModule.emitSource()

      // Process the test annotations.
      for annotation in tc.annotations {
        let code = rtrim(annotation.argument![...])
        switch annotation.command {
        case "cpp":
          check(cxxSource, contains: code, for: tc.name)

        case "hpp":
          check(cxxHeader, contains: code, for: tc.name)

        default:
          XCTFail("\(tc.name): unexpected test command: '\(annotation.command)'")
        }
      }
    })
  }


}
