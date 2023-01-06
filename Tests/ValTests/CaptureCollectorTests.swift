import Core
import XCTest

@testable import FrontEnd

final class CaptureCollectorTests: XCTestCase {

  func testFunctionBindings() throws {
    var ast = AST()
    let module = try ast.insert(wellFormed: ModuleDecl(name: "main"))
    let source = SourceFile(
      contents: """
        fun f<X, v: Void>[let c = ()](_ p: Any) {
          let _ = free   // captured
          let _ = X      // bound
          let _ = v      // bound
          let _ = c      // bound
          let _ = p      // bound
        }
        """)

    let parseDiagnostics = Parser.parse(source, into: module, in: &ast).diagnostics
    XCTAssert(parseDiagnostics.isEmpty, "parsing failed")

    let fun = NodeID<FunctionDecl>(rawValue: ast.topLevelDecls(module).first!.rawValue)
    var collector = CaptureCollector(ast: ast)
    let captures = collector.freeNames(in: fun)

    XCTAssertEqual(captures.count, 2)
    XCTAssert(captures.keys.contains(Name(stem: "free")))
    XCTAssert(captures.keys.contains(Name(stem: "c")))
  }

}
