import XCTest
@testable import Compiler

final class ScopeHierarchyTests: XCTestCase {

  func testBuilder() {
    // Build an AST.
    var ast = AST()
    let main = ast.append(decl: ModuleDecl(name: "main", members: []))
    let trait = ast.append(decl: TraitDecl(
      scopeID: ast[main].makeScopeID(),
      access: nil,
      identifier: SourceRepresentable(node: "T"),
      refinements: [],
      members: [],
      range: nil))
    ast[main].members.append(trait.erased())

    // Build the scope hierarchy of the AST.
    var builder = ScopeHierarchyBuilder()
    let scopes = builder.build(hierarchyOf: main, in: ast)

    XCTAssert(scopes.isContained(ast[trait].scopeID, in: ast[trait].scopeID))
    XCTAssert(scopes.isContained(ast[trait].scopeID, in: ast[main].scopeID))
  }

}
