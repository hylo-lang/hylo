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

    // Subscript the AST for reading with a typed ID.
    let s = a[m].sources.first

    // Subscript the AST with an optional typed ID.
    let typeErasedDecls = try XCTUnwrap(a[s]?.decls)

    // Subscript the AST for reading with a type-erased ID.
    XCTAssert(a[typeErasedDecls.first!] is ImportDecl)
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
