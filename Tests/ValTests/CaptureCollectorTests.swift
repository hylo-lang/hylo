import XCTest
@testable import Compiler

final class CaptureCollectorTests: XCTestCase {

  func testFunctionBindings() {
    var ast = AST()
    let module = ast.insert(ModuleDecl(name: "main"))
    let source = SourceFile(contents: """
      fun f<X, @value v: Void>[let c = ()](_ p: Any) {
        let _ = free   // captured
        let _ = X      // bound
        let _ = v      // bound
        let _ = c      // bound
        let _ = p      // bound
      }
      """)

    let (_, parseDiagnostics) = Parser.parse(source, into: module, in: &ast)
    XCTAssert(parseDiagnostics.isEmpty, "parsing failed")

    let fun = NodeID<FunctionDecl>(rawValue: ast.topLevelDecls(module).first!.rawValue)
    var collector = CaptureCollector(ast: ast)
    let captures = collector.freeNames(in: fun)

    XCTAssertEqual(captures.count, 2)
    XCTAssert(captures.keys.contains(Name(stem: "free")))
    XCTAssert(captures.keys.contains(Name(stem: "c")))
  }

}
