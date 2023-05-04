import Utils
import XCTest

final class ConcatenatedSequenceTests: XCTestCase {

  func testEmptyEmpty() {
    let empty: [Int] = []
    XCTAssert((empty ++ empty).elementsEqual([]))
  }

  func testEmptyPrefix() {
    let empty: [Int] = []
    XCTAssert((empty ++ [1, 2, 3]).elementsEqual([1, 2, 3]))
  }

  func testEmptySuffix() {
    let empty: [Int] = []
    XCTAssert(([1, 2, 3] ++ empty).elementsEqual([1, 2, 3]))
  }

  func testNonEmpty() {
    XCTAssert(([1, 2] ++ [3, 4]).elementsEqual([1, 2, 3, 4]))
  }

  func testUnderestimatedCount() {
    XCTAssertLessThanOrEqual(([1, 2] ++ [3, 4]).underestimatedCount, 4)
  }

  func testIndex() {
    let s = [1, 2] ++ [3, 4]
    XCTAssertEqual(s[s.startIndex], 1)
    XCTAssertEqual(s[s.index(after: s.index(after: s.startIndex))], 3)
  }

}
