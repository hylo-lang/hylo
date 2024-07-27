import FrontEnd
import XCTest

extension XCTestCase {

  /// Parses the hylo file at `hyloFilePath`, `XCTAssert`ing that diagnostics and thrown
  /// errors match annotated expectations.
  @nonobjc
  public func parse(
    _ hyloFilePath: String, extending p: TypedProgram, expectingSuccess expectSuccess: Bool
  ) throws {

    try checkAnnotatedHyloFileDiagnostics(
      inFileAt: hyloFilePath, expectingSuccess: expectSuccess
    ) { (hyloSource, log) in
      var ast = AST()
      _ = try ast.loadModule(
        hyloSource.baseName, sourceCode: [hyloSource], reportingDiagnosticsTo: &log)
    }

  }

}
