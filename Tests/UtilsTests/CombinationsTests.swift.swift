import Utils
import XCTest

final class CombinationsTests: XCTestCase {

  func testIsEmpty() {
    XCTAssertFalse("".combinations(of: 0).isEmpty)
  }

  func testCount() {
    XCTAssertEqual("".combinations(of: 0).count, 1)
    XCTAssertEqual("abc".combinations(of: 1).count, 3)
    XCTAssertEqual("abc".combinations(of: 2).count, 3)
    XCTAssertEqual("abc".combinations(of: 3).count, 1)
  }

  func testUnderstimatedCount() {
    let c = "abc".combinations(of: 2)
    XCTAssertEqual(c.count, c.underestimatedCount)
  }

  func testContents() {
    XCTAssertEqual(Set("".combinations(of: 0)), Set([[]]))
    XCTAssertEqual(
      Set((0 ..< 6).combinations(of: 4)),
      Set([
        [0, 1, 2, 3],
        [0, 1, 2, 4],
        [0, 1, 2, 5],
        [0, 1, 3, 4],
        [0, 1, 3, 5],
        [0, 1, 4, 5],
        [0, 2, 3, 4],
        [0, 2, 3, 5],
        [0, 2, 4, 5],
        [0, 3, 4, 5],
        [1, 2, 3, 4],
        [1, 2, 3, 5],
        [1, 2, 4, 5],
        [1, 3, 4, 5],
        [2, 3, 4, 5],
      ]))
  }

}
