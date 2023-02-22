import Core
import XCTest

@testable import FrontEnd

final class CaptureCollectorTests: XCTestCase {

  func testFunctionBindings() throws {
    let source = SourceFile.diagnosableLiteral(
      """
      fun f<X, v: Void>[let c = ()](_ p: Any) {
        let _ = free   // captured
        let _ = X      // bound
        let _ = v      // bound
        let _ = c      // bound
        let _ = p      // bound
      }
      """)

    var ast = AST()
    let module = try checkNoDiagnostic { diagnostics in
      try ast.makeModule("Test", sourceCode: [source], diagnostics: &diagnostics)
    }

    let fun = FunctionDecl.ID(ast.topLevelDecls(module).first!)!
    var collector = CaptureCollector(ast: ast)
    let captures = collector.freeNames(in: fun)

    XCTAssertEqual(captures.count, 2)
    XCTAssert(captures.keys.contains(Name(stem: "free")))
    XCTAssert(captures.keys.contains(Name(stem: "c")))
  }

}
