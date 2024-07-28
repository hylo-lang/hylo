import FrontEnd
import IR
import StandardLibrary
import Utils
import XCTest

extension XCTestCase {

  /// Lowers the hylo file at `hyloFilePath` to IR, applying any mandatory passes, and `XCTAssert`s
  /// that diagnostics and thrown errors match annotated expectations.
  @nonobjc
  public func lowerToFinishedIR(
    _ hyloFilePath: String, extending p: TypedProgram, expecting expectation: ExpectedTestOutcome
  ) throws {

    try checkAnnotatedHyloFileDiagnostics(
      inFileAt: hyloFilePath, expecting: expectation
    ) { (hyloSource, log) in

      let (p, m) = try p.loadModule(reportingDiagnosticsTo: &log) { (ast, log, space) in
        // Note: built-in module is visible so that we can test built-in function calls.
        try ast.loadModule(
          hyloSource.baseName, parsing: [hyloSource], inNodeSpace: space,
          withBuiltinModuleAccess: true,
          reportingDiagnosticsTo: &log)
      }

      // Emit Hylo IR.
      var ir = try Module(lowering: m, in: p, reportingDiagnosticsTo: &log)
      // Run mandatory IR analysis and transformation passes.
      try ir.applyMandatoryPasses(reportingDiagnosticsTo: &log)
    }

  }

}
