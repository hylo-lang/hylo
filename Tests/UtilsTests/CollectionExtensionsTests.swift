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

}


final class BidirectionalCollectionExtensionsTests: XCTestCase {

  func testSplitAtLastIndexOf() {
    reading([1, 2, 3, 4].split(atLastIndexOf: 3)) { (head, tail) in
      XCTAssertEqual(head, [1, 2])
      XCTAssertEqual(tail, [3, 4])
    }

    reading([3, 3, 3, 3].split(atLastIndexOf: 3)) { (head, tail) in
      XCTAssertEqual(head, [3, 3, 3])
      XCTAssertEqual(tail, [3])
    }

    reading([1, 1, 1, 1].split(atLastIndexOf: 3)) { (head, tail) in
      XCTAssertEqual(head, [])
      XCTAssertEqual(tail, [1, 1, 1, 1])
    }
  }

}
