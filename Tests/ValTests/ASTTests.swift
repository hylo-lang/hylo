import Core
import FrontEnd
import XCTest

final class ASTTests: XCTestCase {

  func testAppendModule() throws {
    var ast = AST()
    var diagnostics = Diagnostics()
    let i = ast.insert(ModuleDecl("Val"), diagnostics: &diagnostics)
    XCTAssert(ast.modules.contains(i))
    XCTAssert(diagnostics.log.isEmpty)
    let j = ast.insert(synthesized: ModuleDecl("Val1"))
    XCTAssert(ast.modules.contains(j))
  }

  func testDeclAccess() throws {
    var ast = AST()

    // Create a module declarations.
    let input = SourceFile(synthesizedText: "")
    let module = ast.insert(synthesized: ModuleDecl("Val"))

    // Create a trait declaration.
    let decl = ast.insert(
      synthesized: ImportDecl(
        introducerSite: input.wholeRange,
        identifier: SourceRepresentable(value: "T", range: input.wholeRange),
        site: input.wholeRange))

    // Create a source declaration set.
    let source = ast.insert(
      synthesized: TranslationUnit(decls: [AnyDeclID(decl)], site: input.wholeRange))
    ast[module].addSourceFile(source)

    // Subscript the AST for reading with a type-erased ID.
    XCTAssert(ast[ast[ast[module].sources.first!].decls.first!] is ImportDecl)
  }

  func testCodableRoundtrip() throws {
    var ast = AST()

    // Create a module declarations.
    let input = SourceFile(synthesizedText: "")
    let module = ast.insert(synthesized: ModuleDecl("Val"))

    let source = ast.insert(
      synthesized: TranslationUnit(
        decls: [
          AnyDeclID(
            ast.insert(
              synthesized: FunctionDecl(
                introducerSite: input.wholeRange,
                identifier: SourceRepresentable(value: "foo", range: input.wholeRange),
                site: input.wholeRange)))
        ],
        site: input.wholeRange))
    ast[module].addSourceFile(source)

    // Serialize the AST.
    let encoder = JSONEncoder().forAST
    let serialized = try encoder.encode(ast)

    // Deserialize the AST.
    let decoder = JSONDecoder().forAST
    let deserialized = try decoder.decode(AST.self, from: serialized)

    // Deserialized AST should containt a function `foo`.
    XCTAssertEqual(deserialized[source].decls.first?.kind, NodeKind(FunctionDecl.self))
  }

}
