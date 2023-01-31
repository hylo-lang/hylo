import Core
import XCTest

@testable import FrontEnd

final class CaptureCollectorTests: XCTestCase {

  func testFunctionBindings() throws {
    var ast = AST()
    let module = ast.insert(synthesized: ModuleDecl(name: "main"))
    let source = testCode(
      """
      fun f<X, v: Void>[let c = ()](_ p: Any) {
        let _ = free   // captured
        let _ = X      // bound
        let _ = v      // bound
        let _ = c      // bound
        let _ = p      // bound
      }
      """)

    var diagnostics = Diagnostics()
    _ = try Parser.parse(source, into: module, in: &ast, diagnostics: &diagnostics)

    let fun = NodeID<FunctionDecl>(ast.topLevelDecls(module).first!)!
    var collector = CaptureCollector(ast: ast)
    let captures = collector.freeNames(in: fun)

    XCTAssertEqual(captures.count, 2)
    XCTAssert(captures.keys.contains(Name(stem: "free")))
    XCTAssert(captures.keys.contains(Name(stem: "c")))
  }

}
