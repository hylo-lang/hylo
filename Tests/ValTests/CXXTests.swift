import XCTest
import Compiler

final class CXXTests: XCTestCase {

  func testTranspiler() throws {
    // Locate the test cases.
    let testCaseDirectory = try XCTUnwrap(
      Bundle.module.url(forResource: "TestCases/CXX", withExtension: nil),
      "No test cases")

    // Prepare an AST with the core module loaded.
    var baseAST = AST()
    try baseAST.importValModule()

    // Execute the test cases.
    try TestCase.executeAll(in: testCaseDirectory, { (tc) in
      // Create an AST for the test case.
      var program = baseAST

      // Create a module for the input.
      let module = program.insert(ModuleDecl(name: tc.name))

      // Parse the input.
      if Parser.parse(tc.source, into: module, in: &program).decls == nil {
        XCTFail("\(tc.name): parsing failed")
        return
      }

      // Run the type checker.
      var checker = TypeChecker(ast: program)
      if !checker.check(module: module) {
        XCTFail("\(tc.name): type checking failed")
        return
      }

      let typedProgram = TypedProgram(
        ast: checker.ast,
        scopeHierarchy: checker.scopeHierarchy,
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
          if specifications.count == 2 {
            XCTAssert(cxxHeader.contains(specifications[0]))
            XCTAssert(cxxSource.contains(specifications[1]))
          } else if specifications.count == 1 {
            XCTAssert(cxxSource.contains(specifications[0]))
          }

        default:
          XCTFail("\(tc.name): unexpected test command: '\(annotation.command)'")
        }
      }
    })
  }


}
