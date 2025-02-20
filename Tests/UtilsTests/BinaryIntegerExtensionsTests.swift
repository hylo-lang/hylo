import Utils
import XCTest

final class BinaryIntegerExtensionsTests: XCTestCase {

  func testRoundedUpToNearestMultipleOf() {
    XCTAssertEqual((-1).rounded(upToNearestMultipleOf: 3), 0)
    XCTAssertEqual((-2).rounded(upToNearestMultipleOf: 3), 0)
    XCTAssertEqual((-3).rounded(upToNearestMultipleOf: 3), -3)
    XCTAssertEqual((-4).rounded(upToNearestMultipleOf: 3), -3)
    XCTAssertEqual((-5).rounded(upToNearestMultipleOf: 3), -3)
    XCTAssertEqual((-6).rounded(upToNearestMultipleOf: 3), -6)

    XCTAssertEqual((-1).rounded(upToNearestMultipleOf: -3), 0)
    XCTAssertEqual((-2).rounded(upToNearestMultipleOf: -3), 0)
    XCTAssertEqual((-3).rounded(upToNearestMultipleOf: -3), -3)
    XCTAssertEqual((-4).rounded(upToNearestMultipleOf: -3), -3)
    XCTAssertEqual((-5).rounded(upToNearestMultipleOf: -3), -3)
    XCTAssertEqual((-6).rounded(upToNearestMultipleOf: -3), -6)

    XCTAssertEqual(0.rounded(upToNearestMultipleOf: 3), 0)

    XCTAssertEqual(1.rounded(upToNearestMultipleOf: 3), 3)
    XCTAssertEqual(2.rounded(upToNearestMultipleOf: 3), 3)
    XCTAssertEqual(3.rounded(upToNearestMultipleOf: 3), 3)
    XCTAssertEqual(4.rounded(upToNearestMultipleOf: 3), 6)
    XCTAssertEqual(5.rounded(upToNearestMultipleOf: 3), 6)
    XCTAssertEqual(6.rounded(upToNearestMultipleOf: 3), 6)

    XCTAssertEqual(1.rounded(upToNearestMultipleOf: -3), 3)
    XCTAssertEqual(2.rounded(upToNearestMultipleOf: -3), 3)
    XCTAssertEqual(3.rounded(upToNearestMultipleOf: -3), 3)
    XCTAssertEqual(4.rounded(upToNearestMultipleOf: -3), 6)
    XCTAssertEqual(5.rounded(upToNearestMultipleOf: -3), 6)
    XCTAssertEqual(6.rounded(upToNearestMultipleOf: -3), 6)

    XCTAssertEqual(0.rounded(upToNearestMultipleOf: 1), 0)
    XCTAssertEqual(5.rounded(upToNearestMultipleOf: 1), 5)
    XCTAssertEqual(5.rounded(upToNearestMultipleOf: 2), 6)
    XCTAssertEqual(5.rounded(upToNearestMultipleOf: 3), 6)
    XCTAssertEqual(5.rounded(upToNearestMultipleOf: 4), 8)
    XCTAssertEqual(5.rounded(upToNearestMultipleOf: 5), 5)
  }

}
