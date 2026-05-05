import FrontEnd
import IR
import Utils
import XCTest

/// Returns a hosted standard library after typechecking and any diagnostics generated.
///
/// The resulting program may be empty if typechecking fails.
private func makeTypecheckedStandardLibrary()
  -> (program: TypedProgram, diagnostics: DiagnosticSet)
{
  var log = DiagnosticSet()
  let p = Utils.Host.hostedLibraryAST
    .map { ScopedProgram($0) }
    .flatMap { p in
      Result { try TypedProgram(annotating: p, reportingDiagnosticsTo: &log) }
    }
  if let e = p.failure {
    guard let d = e as? DiagnosticSet else {
      fatalError(
        "Error thrown from typechecking is a \(type(of: e)), not a DiagnosticSet:\n\(e)")
    }
    precondition(d == log, "Thrown diagnostics don't match mutated ones. \n\(d)\n\(log)")
    return (.empty, log)
  }
  return (p.success!, log)
}

/// The standard library after typechecking, plus any diagnostics generated.
internal let typecheckedStandardLibrary = makeTypecheckedStandardLibrary()

/// A test driver for executing test cases generated from Hylo source files.
///
/// This class is intended to be extended by test cases running Hylo programs through the compiler.
/// It contains the logic for loading and sharing a type checked instance of the standard library
/// across each individual test function, accessible. A unique copy of that shared instance is
/// assigned to `programToExtend` before each test.
///
/// All tests of derived classes are skipped if the loading of the standard library fails.
open class HyloTestCase: XCTestCase {

  /// A shared program instance containing only the standard library.
  private static let base = SharedMutable<TypedProgram?>(nil)

  /// The program being processed by the test case.
  ///
  /// This property is assigned before each test to a fresh copy of `base` and is intended to be
  /// extended with the code under test.
  public var programToExtend: TypedProgram?

  /// Returns a copy of `base`, creating it if necessary.
  private func checkedBaseProgram() throws -> TypedProgram {
    return try self.checkNoDiagnostic { (d) in
      assert(d.isEmpty)
      d = typecheckedStandardLibrary.diagnostics
      try d.throwOnError()
      return typecheckedStandardLibrary.program
    }
  }

  /// Assigns `programToExtend` to a copy of the shared instance or throws `XCTSkip`.
  open override func setUpWithError() throws {
    do {
      programToExtend = try HyloTestCase.base.modify(applying: { (p) in
        try p.setIfNil(checkedBaseProgram())
      })
    } catch _ {
      throw XCTSkip()
    }
  }

  /// Destroys `programToExtend`.
  open override func tearDown() {
    programToExtend = nil
  }

}
