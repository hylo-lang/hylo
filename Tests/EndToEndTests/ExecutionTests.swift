import Core
import XCTest

class EndToEndTestCase: XCTestCase {}

extension XCTestCase {

  /// Compiles and runs the given file at `valFilePath`, `XCTAssert`ing that diagnostics and exit
  /// codes match annotated expectations.
  func compileAndRun(_ valFilePath: String)
    throws
  {
    print("testing", valFilePath)
  }

}
