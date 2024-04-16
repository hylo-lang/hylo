import FrontEnd
import XCTest

extension UnrecognizedSelectorWorkaroundTestCase {

  /// Parses the hylo file at `hyloFilePath`, `XCTAssert`ing that diagnostics and thrown
  /// errors match annotated expectations.
  public func parse(_ hyloFilePath: String, expectSuccess: Bool) throws {

    try checkAnnotatedHyloFileDiagnostics(inFileAt: hyloFilePath, expectSuccess: expectSuccess) {
      (hyloSource, diagnostics) in
      var ast = AST()
      _ = try ast.makeModule(
        hyloSource.baseName, sourceCode: [hyloSource], diagnostics: &diagnostics)
    }

  }

}
