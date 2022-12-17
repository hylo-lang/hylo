import FrontEnd
import XCTest
import Core

final class ASTTests: XCTestCase {

  func testAppendModule() throws {
    var ast = AST()
    let i = try ast.insert(wellFormed: ModuleDecl(name: "Val"))
    XCTAssert(ast.modules.contains(i))
  }

  func testDeclAccess() throws {
    var ast = AST()

    // Create a module declarations.
    let module = try ast.insert(wellFormed: ModuleDecl(name: "Val"))

    // Create a trait declaration.
    let trait = try ast.insert(
      wellFormed: TraitDecl(
        accessModifier: nil,
        identifier: SourceRepresentable(value: "T"),
        refinements: [],
        members: [],
        origin: nil))

    // Create a source declaration set.
    let source = try ast.insert(wellFormed: TopLevelDeclSet(decls: [AnyDeclID(trait)]))
    ast[module].addSourceFile(source)

    // Subscript the AST for reading with a type-erased ID.
    XCTAssert(ast[ast[ast[module].sources.first!].decls.first!] is TraitDecl)
  }

  func testCodableRoundtrip() throws {
    var ast = AST()

    // Create a module.
    let module = try ast.insert(wellFormed: ModuleDecl(name: "Val"))
    let source = try ast.insert(
      wellFormed: TopLevelDeclSet(
        decls: [
          AnyDeclID(
            ast.insert(
              wellFormed: FunctionDecl(
                introducerRange: nil,
                identifier: SourceRepresentable(value: "foo"),
                origin: nil)))
        ]))
    ast[module].addSourceFile(source)

    // Serialize the AST.
    let encoder = JSONEncoder()
    let serialized = try encoder.encode(ast)

    // Deserialize the AST.
    let decoder = JSONDecoder()
    let deserialized = try decoder.decode(AST.self, from: serialized)

    // Deserialized AST should containt a function `foo`.
    XCTAssertEqual(deserialized[source].decls.first?.kind, NodeKind(FunctionDecl.self))
  }

}
