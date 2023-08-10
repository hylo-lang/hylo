import Utils
import XCTest

final class ChunksTests: XCTestCase {

  func testGreaterNumberOfChunks() {
    let a = 0 ..< 10
    let b = a.chunked(inMax: 12)
    XCTAssertEqual(b.count, 10)
    XCTAssert(b.allSatisfy({ $0.count == 1 }))
    XCTAssert(b.compactMap(\.first).elementsEqual(a))
  }

  func testSmallerNumberOfEvenChunks() {
    let a = 0 ..< 10
    let b = a.chunked(inMax: 5)
    XCTAssertEqual(b.count, 5)
    XCTAssert(b.allSatisfy({ $0.count == 2 }))
    XCTAssert(b.map({ $0[...] }).joined().elementsEqual(a))
  }

  func testSmallerNumberOfUnevenChunks() {
    let a = 0 ..< 10
    let b = a.chunked(inMax: 4)
    XCTAssertEqual(b.count, 4)
    XCTAssert(b.dropLast().allSatisfy({ $0.count == 3 }))
    XCTAssert(b.map({ $0[...] }).joined().elementsEqual(a))
  }

  func testJoined() {
    let a = 0 ..< 10
    XCTAssertEqual(a.chunked(inMax: 3).joined(), a)
  }

}
