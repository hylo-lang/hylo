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
      """

    let input = SourceFile(synthesizedText: text, named: "main")
    let (p, m) = try checkNoDiagnostic { (d) in
      var a = AST.standardLibrary
      let m = try a.makeModule("Main", sourceCode: [input], diagnostics: &d)
      return (try TypedProgram(a, diagnostics: &d), m)
    }

    var o = Observer(forNodesIn: p)
    p.ast.walk(m, notifying: &o)
  }

}

private struct Observer: ASTWalkObserver {

  /// The program containing the visited AST nodes.
  let program: TypedProgram

  /// The line at which this instance has been created.
  let diagnosticLine: UInt

  /// Creates an instance observing the nodes in `p` and reporting diagnostics as though errors
  /// occured at line `l` of this source file.
  init(forNodesIn p: TypedProgram, reportingDiagnosticsAtLine l: UInt = #line) {
    self.program = p
    self.diagnosticLine = l
  }

  func willEnter(_ n: AnyNodeID, in ast: AST) -> Bool {
    if let d = AnyDeclID(n) {
      let mangled = program.mangled(d)

      guard let demangled = program.demangle(mangled) else {
        XCTFail("could not demangle '\(mangled)'", line: diagnosticLine)
        return false
      }

      XCTAssertEqual(demangled.node, n, line: diagnosticLine)
    }

    return true
  }

}
