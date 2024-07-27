import FrontEnd
import StandardLibrary
import Utils
import XCTest

extension XCTestCase {

  /// Type-checks the Hylo file at `hyloFilePath`, `XCTAssert`ing that diagnostics and thrown
  /// errors match annotated expectations.
  @nonobjc
  public func typeCheck(
    _ hyloFilePath: String, extending p: TypedProgram, expectingSuccess expectSuccess: Bool
  ) throws {

    try checkAnnotatedHyloFileDiagnostics(
      inFileAt: hyloFilePath, expectingSuccess: expectSuccess
    ) { (hyloSource, log) in
      _ = try p.loadModule(reportingDiagnosticsTo: &log) { (ast, log, space) in
        try ast.loadModule(
          hyloSource.baseName, sourceCode: [hyloSource], builtinModuleAccess: true,
          reportingDiagnosticsTo: &log)
      }
    }

  }

}
