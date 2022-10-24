import XCTest
import Compiler
import Utils

final class TypeCheckerNewTests: XCTestCase {

  func testTypeChecker() throws {
    // Prepare an AST with the core module loaded.
    var baseAST = AST()
    try baseAST.importValModule()

    // Collect the URLs of the test cases for the type checker.
    let urls = Bundle.module.urls(
      forResourcesWithExtension: "val",
      subdirectory: "TestCases/TypeChecking") ?? []

    for url in urls {
      let tc = TestCase(url: url)
      try tc.execute({ (input, annotations) in
        // Create an AST for the test case.
        var program = baseAST

        // Create a module for the input.
        let module = program.insert(ModuleDecl(name: tc.name))

        // Parse the input.
        if Parser.parse(input, into: module, in: &program).decls == nil {
          XCTFail("\(tc.name): parsing failed")
          return
        }

        // Run the type checker.
        var checker = TypeChecker(ast: program)
        let success = checker.check(module: module)

        // Rearrange the diagnostics for faster lookup.
        var diagnostics: [Int?: [Diagnostic]] = checker.diagnostics.reduce(into: [:], { (ds, d) in
          if let l = d.location, l.source == input {
            let (line, _) = l.source.lineAndColumnIndices(at: l)
            ds[line, default: []].append(d)
          } else {
            ds[nil, default: []].append(d)
          }
        })

        // Process the test annotations.
        for annotation in annotations {
          switch annotation.command {
          case "expect-failure":
            XCTAssertFalse(success, "\(tc.name): type checking succeeded, but expected failure")

          case "expect-success":
            XCTAssertTrue(success, "\(tc.name): type checking failed, but expected success")

          case "diagnostic":
            var ds = diagnostics[annotation.line, default: []]
            guard let i = ds.firstIndex(where: { $0.message == annotation.argument }) else {
              XCTFail("\(tc.name): missing expected diagnostic at line \(annotation.line)")
              continue
            }

            // Remove the diagnostic from the set.
            ds.remove(at: i)
            diagnostics[annotation.line] = ds.isEmpty ? nil : ds

          default:
            print("\(tc.name): unexpected test command: '\(annotation.command)'")
          }
        }

        // Fail the test for any unexpected diagnostic.
        XCTAssert(diagnostics.isEmpty, "\(tc.name): \(diagnostics.count) unexpected diagnostic(s)")
      })
    }
  }

}
