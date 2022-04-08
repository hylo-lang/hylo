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

    // Create a module declarations.
    let i = ast.append(decl: ModuleDecl(name: "Val", members: []))

    // Create a trait declaration, subscripting the AST for writing with a typed index.
    let j = ast.append(decl: TraitDecl(
      scopeID: ast[i].makeScopeID(),
      access: nil,
      identifier: SourceRepresentable(node: "T"),
      refinements: [],
      members: [],
      range: nil))
    ast[i].members.append(j.erased())

    // Subscript the AST for reading with a type-erased index.
    let t = try XCTUnwrap(ast[ast[i].members.first!] as? TraitDecl)
    XCTAssert(t.identifier.node == "T")
  }

}
