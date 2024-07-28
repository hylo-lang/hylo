import FrontEnd
import XCTest

extension XCTestCase {

  /// Parses the hylo file at `hyloFilePath`, `XCTAssert`ing that diagnostics and thrown
  /// errors match annotated expectations.
  @nonobjc
  public func parse(
    _ hyloFilePath: String, extending p: TypedProgram, expecting expectation: ExpectedTestOutcome
  ) throws {

    try checkAnnotatedHyloFileDiagnostics(
      inFileAt: hyloFilePath, expecting: expectation
    ) { (hyloSource, log) in
      var ast = AST()
      _ = try ast.loadModule(
        hyloSource.baseName, parsing: [hyloSource], reportingDiagnosticsTo: &log)
    }

  }

}
