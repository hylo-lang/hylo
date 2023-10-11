import Utils
import XCTest

final class CollectionExtensionsTests: XCTestCase {

  func testPartitionIndexAt() {
    XCTAssertEqual([].partitioningIndex(at: 42), 0)

    XCTAssertEqual([0].partitioningIndex(at: 0), 1)
    XCTAssertEqual([0].partitioningIndex(at: 1), 1)

    XCTAssertEqual([1].partitioningIndex(at: 0), 0)
    XCTAssertEqual([1].partitioningIndex(at: 1), 1)

    XCTAssertEqual([0, 2].partitioningIndex(at: 0), 1)
    XCTAssertEqual([0, 2].partitioningIndex(at: 1), 1)
    XCTAssertEqual([0, 2].partitioningIndex(at: 2), 2)
    XCTAssertEqual([0, 2].partitioningIndex(at: 3), 2)
  }

  func testMinimalElements() {
    XCTAssertEqual([Int]().minimalElements(by: compare(_:_:)), [])
    XCTAssertEqual([2].minimalElements(by: compare(_:_:)), [2])

    let x = [4, 8, 3, 2, 5, 6]
    XCTAssertEqual(Set(x.minimalElements(by: compare(_:_:))), [2, 3, 5])

    /// Returns whether `a` is divisor of `b` or vice versa.
    func compare(_ a: Int, _ b: Int) -> StrictPartialOrdering {
      if a == b {
        return .equal
      } else if a % b == 0 {
        return .descending
      } else if b % a == 0 {
        return .ascending
      } else {
        return nil
      }
    }
  }

}

final class BidirectionalCollectionExtensionsTests: XCTestCase {

  func testSplitAtLastIndexOf() {
    read([1, 2, 3, 4].split(atLastIndexOf: 3)) { (head, tail) in
      XCTAssertEqual(head, [1, 2])
      XCTAssertEqual(tail, [3, 4])
    }

    read([3, 3, 3, 3].split(atLastIndexOf: 3)) { (head, tail) in
      XCTAssertEqual(head, [3, 3, 3])
      XCTAssertEqual(tail, [3])
    }

    read([1, 1, 1, 1].split(atLastIndexOf: 3)) { (head, tail) in
      XCTAssertEqual(head, [])
      XCTAssertEqual(tail, [1, 1, 1, 1])
    }
  }

}

final class MutableCollectionExtensionsTests: XCTestCase {

  func testRotateRange() {
    var a = Array(0 ..< 7)
    XCTAssertEqual(a.rotate(0 ..< 5, toStartAt: 3), 2)
    XCTAssertEqual(a, [3, 4, 0, 1, 2, 5, 6])

    let b = a
    XCTAssertEqual(a.rotate(0 ..< 1, toStartAt: 0), 0)
    XCTAssertEqual(a, b)
  }

  func testRotateWhole() {
    var a = Array(0 ..< 7)
    XCTAssertEqual(a.rotate(0 ..< 7, toStartAt: 4), 3)
    XCTAssertEqual(a, [4, 5, 6, 0, 1, 2, 3])

    let b = a
    XCTAssertEqual(a.rotate(toStartAt: 0), 0)
    XCTAssertEqual(a, b)
  }

}
