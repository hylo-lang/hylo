import XCTest
@testable import Compiler

final class ScopeHierarchyTests: XCTestCase {

  func testBuilder() {
    // Build an AST.
    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main", members: []))
    let trait = ast.insert(TraitDecl(
      accessModifier: nil,
      identifier: SourceRepresentable(value: "T"),
      refinements: [],
      members: []))
    ast[main].members.append(AnyDeclID(trait))

    // Build the scope hierarchy of the AST.
    let hierarchy = ast.scopeHierarchy()

    XCTAssert(hierarchy.isContained(trait, in: trait))
    XCTAssert(hierarchy.isContained(trait, in: main))
    XCTAssertFalse(hierarchy.isContained(main, in: trait))

    XCTAssert(hierarchy.container[trait].map({ $0 == main }) ?? false)
    XCTAssert(hierarchy.containees[main, default: []].contains(where: { $0 == trait }))
  }

}
