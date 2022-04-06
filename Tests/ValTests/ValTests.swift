import XCTest
@testable import Compiler

final class ASTTests: XCTestCase {

  func testAppendModule() {
    var ast = AST()
    let i = ast.append(decl: ModuleDecl(name: "Val", members: []))
    XCTAssert(ast.modules.contains(i))
  }

  func testDeclAccess() throws {
    var ast = AST()

    // Create two declarations.
    let i = ast.append(decl: ModuleDecl(name: "Val", members: []))
    let j = ast.append(decl: TraitDecl(
      range: nil,
      access: nil,
      identifier: Identifier(range: nil, value: "T"),
      refinements: [],
      members: []))

    // Subscript the AST for writing with a typed index.
    ast[i].members.append(j.erased())

    // Subscript the AST for reading with a type-erased index.
    let t = try XCTUnwrap(ast[ast[i].members.first!] as? TraitDecl)
    XCTAssert(t.identifier.value == "T")
  }

}
