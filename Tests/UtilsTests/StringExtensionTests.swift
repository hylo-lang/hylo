import Utils
import XCTest

final class StringExtensionsTests: XCTestCase {

  func testRemovingSuffix() {
    XCTAssertEqual(String("abc".removingSuffix("")), "abc")
    XCTAssertEqual(String("abc".removingSuffix("c")), "ab")
    XCTAssertEqual(String("abc".removingSuffix("bc")), "a")
    XCTAssertEqual(String("abc".removingSuffix("abc")), "")
    XCTAssertEqual(String("abc".removingSuffix("ab")), "abc")
  }

  func testSnakeCase() {
    XCTAssertEqual("test".snakeCased(), "test")
    XCTAssertEqual("testCase".snakeCased(), "test_case")
    XCTAssertEqual("Foundation".snakeCased(), "foundation")
    XCTAssertEqual("FileManager".snakeCased(), "file_manager")
    XCTAssertEqual("URLResourceKey".snakeCased(), "url_resource_key")
  }

}
