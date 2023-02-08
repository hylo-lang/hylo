import Core
import FrontEnd
import XCTest

final class ASTTests: XCTestCase {

  func testAppendModule() throws {
    var ast = AST()
    var diagnostics = Diagnostics()
    let i = ast.insert(ModuleDecl(name: "Val", sources: []), diagnostics: &diagnostics)
    XCTAssert(ast.modules.contains(i))
    XCTAssert(diagnostics.log.isEmpty)

    let j = ast.insert(synthesized: ModuleDecl(name: "Val1", sources: []))
    XCTAssert(ast.modules.contains(j))
  }

  func testDeclAccess() throws {
    let input = testCode(
      """
      import T
      """)

    var a = AST()
    var d = Diagnostics()
    let m = try a.makeModule("Main", sourceCode: [input], diagnostics: &d)
    XCTAssert(d.log.isEmpty, "\n\(d)")

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
    let input = testCode(
      """
      public fun main() {
        print("Hello, World!")
      }
      """)

    var original = AST()
    var d = Diagnostics()
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
