import Core
import XCTest

final class MangledStringTests: XCTestCase {

  func testEmpty() {
    let s = MangledString("")
    XCTAssertEqual(s.rawValue, "_")
    XCTAssertEqual(s.description, "")
  }

  func testNoSanitization() {
    XCTAssertEqual(MangledString("abc123").description, "abc123")
    XCTAssertEqual(MangledString("_foo").description, "_foo")
  }

  func testASCII() {
    XCTAssertEqual(MangledString("Hello, World!").description, "Hello, World!")
    XCTAssertEqual(MangledString("infix+++").description, "infix+++")
  }

  func testNonASCII() {
    XCTAssertEqual(MangledString("Français").description, "Français")
    XCTAssertEqual(MangledString("日本語").description, "日本語")
  }

}
