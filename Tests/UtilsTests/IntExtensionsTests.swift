import Utils
import XCTest

final class IntExtensionsTests: XCTestCase {

  func testRoundUpToNearestMultipleOf() {
    XCTAssertEqual(0.round(upToNearestMultipleOf: 1), 0)
    XCTAssertEqual(5.round(upToNearestMultipleOf: 1), 5)
    XCTAssertEqual(5.round(upToNearestMultipleOf: 2), 6)
    XCTAssertEqual(5.round(upToNearestMultipleOf: 3), 6)
    XCTAssertEqual(5.round(upToNearestMultipleOf: 4), 8)
    XCTAssertEqual(5.round(upToNearestMultipleOf: 5), 5)
  }

}
