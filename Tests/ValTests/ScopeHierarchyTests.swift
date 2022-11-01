import XCTest
@testable import Compiler

final class ScopeHierarchyTests: XCTestCase {

  func testBuilder() {
    // Build an AST.
    var ast = AST()
    let main = ast.insert(ModuleDecl(name: "main"))

    let trait = ast.insert(TraitDecl(
      identifier: SourceRepresentable(value: "T"),
      refinements: [],
      members: []))
    let source = ast.insert(TopLevelDeclSet(decls: [AnyDeclID(trait)]))
    ast[main].addSourceFile(source)

    // Build the scope hierarchy of the AST.
    let hierarchy = ast.scopeHierarchy()

    XCTAssert(hierarchy.isContained(trait, in: trait))
    XCTAssert(hierarchy.isContained(trait, in: main))
    XCTAssertFalse(hierarchy.isContained(main, in: trait))

    XCTAssertEqual(hierarchy.container[trait], AnyScopeID(source))
    XCTAssert(hierarchy.containees[source, default: []].contains(where: { $0 == trait }))
  }

}
