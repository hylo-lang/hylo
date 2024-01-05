import Utils
import XCTest
import Algorithms

extension Sequence {

  /// Returns the elements of this sequence concatenated with the elements in `tail`.
  public func concatenated<Tail>(with tail: Tail) -> Chain2Sequence<Self, Tail> {
    chain(self, tail)
  }

  /// Returns the concatenation of `head` and `tail`.
  public static func ++ <Tail>(head: Self, tail: Tail) -> Chain2Sequence<Self, Tail> {
    head.concatenated(with: tail)
  }

}

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
