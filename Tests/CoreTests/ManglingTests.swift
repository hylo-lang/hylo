import Core
import FrontEnd
import TestUtils
import XCTest

final class ManglingTests: XCTestCase {

  func testRoundtrip() throws {
    let text = """
      namespace N {
        type A {}
      }

      fun foo(_ x: Int, label y: N.A) -> Int { 0 }

      fun français() {}
      """

    let input = SourceFile(synthesizedText: text, named: "main")
    let (p, m) = try checkNoDiagnostic { (d) in
      var a = AST.standardLibrary
      let m = try a.makeModule("Main", sourceCode: [input], diagnostics: &d)
      return (try TypedProgram(a, diagnostics: &d), m)
    }

    var o = SymbolCollector(forNodesIn: p)
    p.ast.walk(m, notifying: &o)

    var expected: Set = [
      "Main",
      "Main.N",
      "Main.N.A",
      "Main.foo(_:label:)",
      "Main.français()",
    ]

    for m in o.symbols.keys {
      let demangled = try XCTUnwrap(DemangledSymbol(m), "unable to demangle \"\(m)\"")
      expected.remove(demangled.description)
    }
    XCTAssert(expected.isEmpty, "symbols not found: \(list: expected)")
  }

}

/// An AST visitation callback that collects mangled symbols, asserting that they are unique.
private struct SymbolCollector: ASTWalkObserver {

  /// The program containing the visited AST nodes.
  let program: TypedProgram

  /// The line at which this instance has been created.
  let failuresReportingLine: UInt

  /// A table mapping mangled symbols to their source.
  private(set) var symbols: [String: AnyDeclID] = [:]

  /// Creates an instance observing the nodes in `p` and reporting assertion failures as though
  /// they occured at line `l` of this source file..
  init(forNodesIn p: TypedProgram, reportingFailuresAtLine l: UInt = #line) {
    self.program = p
    self.failuresReportingLine = l
  }

  mutating func willEnter(_ n: AnyNodeID, in ast: AST) -> Bool {
    if let d = AnyDeclID(n) {
      let s = program.mangled(d)
      let k = symbols.updateValue(d, forKey: s)
      XCTAssert(
        k == nil, "mangled representation of \(d.kind) collides with \(k!.kind)",
        line: failuresReportingLine)
    }

    return true
  }

}
