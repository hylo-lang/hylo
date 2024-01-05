import Utils
import XCTest
import Algorithms

final class CombinationsTests: XCTestCase {

  func testCount() {
    XCTAssertEqual("".combinations(ofCount: 0).count, 1)
    XCTAssertEqual("abc".combinations(ofCount: 1).count, 3)
    XCTAssertEqual("abc".combinations(ofCount: 2).count, 3)
    XCTAssertEqual("abc".combinations(ofCount: 3).count, 1)
  }

  func testContents() {
    XCTAssertEqual(Set("".combinations(ofCount: 0)), Set([[]]))
    XCTAssertEqual(
      Set((0 ..< 6).combinations(ofCount: 4)),
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
