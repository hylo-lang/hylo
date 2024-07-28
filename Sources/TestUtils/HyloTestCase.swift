import FrontEnd
import IR
import Utils
import XCTest

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
  private static var base = SharedMutable<TypedProgram?>(nil)

  /// The program being processed by the test case.
  ///
  /// This property is assigned before each test to a fresh copy of `base` and is intended to be
  /// extended with the code under test.
  public var programToExtend: TypedProgram?

  /// Returns a copy of `base`, creating it if necessary.
  private func checkedBaseProgram() throws -> TypedProgram {
    return try self.checkNoDiagnostic { (d) in
      let a = try Utils.Host.hostedLibraryAST.get()
      let b = ScopedProgram(a)
      return try TypedProgram(annotating: b, reportingDiagnosticsTo: &d)
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
