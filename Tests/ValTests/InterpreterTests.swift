import Core
import FrontEnd
import IR
import Utils
import XCTest

final class InterpreterTests: XCTestCase {
  func testSimple() throws {
    let input = SourceFile.diagnosableLiteral(
      """
      public fun main() {
      }
      """)

    var syntax = AST.coreModule
    try checkNoDiagnostic { d in
      let mainSyntax = try syntax.makeModule("Main", sourceCode: [input], diagnostics: &d)

      let mainIR = try Module(
        lowering: mainSyntax, in: TypedProgram(syntax, diagnostics: &d),
        diagnostics: &d)

      _ = mainIR
    }
  }
}
