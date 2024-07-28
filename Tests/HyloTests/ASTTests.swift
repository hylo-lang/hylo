import FrontEnd
import StandardLibrary
import Utils
import XCTest
import TestUtils

final class ASTTests: XCTestCase {

  func testAppendModule() throws {
    var ast = AST()
    var log = DiagnosticSet()
    let i = ast.loadModule(reportingDiagnosticsTo: &log) { (tree, _, k) in
      tree.insert(synthesized: ModuleDecl("A", sources: []), inNodeSpace: k)
    }
    XCTAssert(ast.modules.contains(i))

    let j = ast.loadModule(reportingDiagnosticsTo: &log) { (tree, _, k) in
      tree.insert(synthesized: ModuleDecl("B", sources: []), inNodeSpace: k)
    }
    XCTAssert(ast.modules.contains(j))
  }

  func testDeclAccess() throws {
    let input: SourceFile = "import T"

    var a = AST()
    let m = try checkNoDiagnostic { (d) in
      try a.loadModule("Main", parsing: [input], reportingDiagnosticsTo: &d)
    }

    // Note: we use `XCTUnwrap` when we're expecting a non-nil value produced by a subscript under
    // test. Otherwise, we use `!`.

    // Test `AST.subscript<T: ConcreteNodeID>(T)`
    let s = a[m].sources.first

    // Test `AST.subscript<T: ConcreteNodeID>(T?)`
    let allDecls = try XCTUnwrap(a[s]?.decls)

    // Test `AST.subscript<T: NodeIDProtocol>(T)`
    XCTAssert(a[allDecls.first!] is ImportDecl)

    // Test `AST.subscript<T: NodeIDProtocol>(T?)`
    XCTAssert(a[allDecls.first] is ImportDecl)
  }

  func testCodableRoundtrip() throws {
    try checkNoDiagnostic { (d) in
      let serialized = try JSONEncoder().forAST.encode(Host.hostedLibraryAST.get())
      let deserialized = try JSONDecoder().forAST.decode(AST.self, from: serialized)
      // Creation of TypedProgram validates that it contains well-formed coreTraits.
      _ = try TypedProgram(annotating: ScopedProgram(deserialized), reportingDiagnosticsTo: &d)
    }
  }

  func testWalk() throws {
    let input: SourceFile = """
      fun f() -> Int { fun ff() {}; return 42 }
      type A {
        fun g() {}
      }
      """

    var a = AST()
    let m = try checkNoDiagnostic { (d) in
      try a.loadModule("Main", parsing: [input], reportingDiagnosticsTo: &d)
    }

    struct V: ASTWalkObserver {

      var outermostFunctions: [FunctionDecl.ID] = []

      mutating func willEnter(_ n: AnyNodeID, in ast: AST) -> Bool {
        if let d = FunctionDecl.ID(n) {
          outermostFunctions.append(d)
          return false
        } else {
          return true
        }
      }

    }

    var v = V()
    a.walk(m, notifying: &v)
    XCTAssertEqual(v.outermostFunctions.count, 2)
  }

}
