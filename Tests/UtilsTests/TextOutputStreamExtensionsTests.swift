import Utils
import XCTest

final class TextOutputStreamExtensionsTests: XCTestCase {

  func testWriteContentsOf() {
    let numbers = 0 ..< 5
    var result = ""
    result.write(contentsOf: numbers, separatedBy: ".") { (s, n) in s.write("\(n)") }
    XCTAssertEqual(result, "0.1.2.3.4")
  }

}
