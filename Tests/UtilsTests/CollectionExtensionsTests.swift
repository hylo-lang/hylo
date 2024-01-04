import Utils
import XCTest
import Algorithms

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
    for l in 0..<11 {
      let a = Array(0..<l)

      for p in a.startIndex...a.endIndex {
        let prefix = a[..<p]
        for q in p...l {
          let suffix = a[q...]

          for m in p ... q {
            var b = a

            let r = b[p..<q].rotate(subrange: p ..< q, toStartAt: m)
            let rotated = Array([prefix, a[m..<q], a[p..<m], suffix].joined())
            XCTAssertEqual(b, rotated)
            XCTAssertEqual(r, a.index(p, offsetBy: a[m..<q].count))
          }
        }
      }
    }
  }

}
