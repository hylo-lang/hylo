import XCTest

@testable import CodeGenCXX

final class CXXIdentifierTests: XCTestCase {

  func testIdentity() {
    XCTAssertEqual(CXXIdentifier("foo").description, "foo")
    XCTAssertEqual(CXXIdentifier("_bar").description, "_bar")
  }

  func testReservedKeyword() {
    XCTAssertEqual(CXXIdentifier("template").description, "_template")
  }

  func testOperator() {
    XCTAssertEqual(CXXIdentifier("++").description, "u2bu2b")
  }

}
