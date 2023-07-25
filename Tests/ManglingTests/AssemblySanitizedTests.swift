import IR
import XCTest

final class AssemblySanitizedTests: XCTestCase {

  func testIsAssemblySuitable() {
    XCTAssert("".isAssemblySuitable)
    XCTAssert("_foo".isAssemblySuitable)

    XCTAssertFalse("étoile".isAssemblySuitable)
    XCTAssertFalse("Bang!".isAssemblySuitable)
    XCTAssertFalse("谷".isAssemblySuitable)
  }

  func testEmpty() {
    let s = "".assemblySanitized
    XCTAssertEqual(s, "")
    XCTAssertEqual(String(assemblySanitized: s), "")
  }

  func testNoSanitization() {
    XCTAssertEqual("abc123".assemblySanitized, "abc123")
    XCTAssertEqual("_foo".assemblySanitized, "_foo")
  }

  func testASCII() {
    let s = "infix+++".assemblySanitized
    XCTAssert(s.isAssemblySuitable)
    XCTAssertEqual(String(assemblySanitized: s), "infix+++")
  }

  func testNonASCII() {
    let s = "日本語".assemblySanitized
    XCTAssert(s.isAssemblySuitable)
    XCTAssertEqual(String(assemblySanitized: s), "日本語")
  }

}
