import Utils
import XCTest

final class SequenceExtensionsTests: XCTestCase {

  func testElementCount() {
    XCTAssertEqual([].elementCount(where: { $0 > 0 }), 0)
    XCTAssertEqual([0].elementCount(where: { $0 > 0 }), 0)
    XCTAssertEqual([0, 0, 0].elementCount(where: { $0 > 0 }), 0)
    XCTAssertEqual([0, 2, 0].elementCount(where: { $0 > 0 }), 1)
    XCTAssertEqual([2, 0, 2].elementCount(where: { $0 > 0 }), 2)
  }

}
