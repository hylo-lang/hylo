import XCTest
@testable import Compiler

final class ASTTests: XCTestCase {

  func testAppendModule() {
    var ast = AST()
    let i = ast.insert(ModuleDecl(name: "Val", members: []))
    XCTAssert(ast.modules.contains(i))
  }

  func testDeclAccess() throws {
    var ast = AST()

    // Create a module declarations.
    let i = ast.insert(ModuleDecl(name: "Val", members: []))

    // Create a trait declaration, subscripting the AST for writing with a typed index.
    let j = ast.insert(TraitDecl(
      access: nil,
      identifier: SourceRepresentable(value: "T"),
      refinements: [],
      members: []))
    ast[i].members.append(AnyDeclIndex(j))

    // Subscript the AST for reading with a type-erased index.
    let t = try XCTUnwrap(ast[ast[i].members.first!] as? TraitDecl)
    XCTAssert(t.identifier.value == "T")
  }

}
