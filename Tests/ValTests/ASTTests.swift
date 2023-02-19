import Core
import FrontEnd
import XCTest

final class ASTTests: XCTestCase {

  func testAppendModule() throws {
    var ast = AST()
    let i = checkNoDiagnostic { diagnostics in
      ast.insert(ModuleDecl("Val", sources: []), diagnostics: &diagnostics)
    }
    XCTAssert(ast.modules.contains(i))

    let j = ast.insert(synthesized: ModuleDecl("Val1", sources: []))
    XCTAssert(ast.modules.contains(j))
  }

  func testDeclAccess() throws {
    let input: SourceFile = "import T"

    var a = AST()

    let m = try checkNoDiagnostic { d in
      try a.makeModule("Main", sourceCode: [input], diagnostics: &d)
    }

    // Note: we use `XCTUnwrap` when we're expecting a non-nil value produced by a subscript under
    // test. Otherwise, we use `!`.

    // Test `AST.subscript<T: ConcreteNodeID>(T)`
    let s = a[m].sources.first

    // Test `AST.subscript<T: ConcreteNodeID>(T?)`
    let allDecls = try XCTUnwrap(a[s]?.decls)

    // Test `AST.subscript<T: NodeIDProtocol>(T)`
    XCTAssert(a[allDecls.first!] is ImportDecl)

    // Test `AST.subscript<T: NodeIDProtocol>(T?)`
    XCTAssert(a[allDecls.first] is ImportDecl)
  }

  func testCodableRoundtrip() throws {
    let input: SourceFile = """
      public fun main() {
        print("Hello, World!")
      }
      """

    var original = AST()
    var d = DiagnosticSet()
    let m = try original.makeModule("Main", sourceCode: [input], diagnostics: &d)

    // Serialize the AST.
    let encoder = JSONEncoder().forAST
    let serialized = try encoder.encode(original)

    // Deserialize the AST.
    let decoder = JSONDecoder().forAST
    let deserialized = try decoder.decode(AST.self, from: serialized)

    // Deserialized AST should contain a `main` function.
    let s = deserialized[m].sources.first!
    XCTAssertEqual(deserialized[s].decls.first?.kind, NodeKind(FunctionDecl.self))
  }

}
