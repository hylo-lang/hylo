import XCTest
import Compiler

final class ASTTests: XCTestCase {

  func testAppendModule() {
    var ast = AST()
    let i = ast.insert(ModuleDecl(name: "Val"))
    XCTAssert(ast.modules.contains(i))
  }

  func testDeclAccess() throws {
    var ast = AST()

    // Create a module declarations.
    let module = ast.insert(ModuleDecl(name: "Val"))

    // Create a trait declaration.
    let trait = ast.insert(TraitDecl(
      accessModifier: nil,
      identifier: SourceRepresentable(value: "T"),
      refinements: [],
      members: []))

    // Create a source declaration set.
    let source = ast.insert(TopLevelDeclSet(decls: [AnyDeclID(trait)]))
    ast[module].addSourceFile(source)

    // Subscript the AST for reading with a type-erased ID.
    XCTAssert(ast[ast[ast[module].sources.first!].decls.first!] is TraitDecl)
  }

}
