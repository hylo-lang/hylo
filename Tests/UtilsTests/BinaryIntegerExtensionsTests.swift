import Utils
import XCTest

extension BinaryInteger {

  /// Returns `self` rounded up to the nearest multiple of `n`.
  ///
  /// Requires: `n` is nonzero
  public func rounded(upToNearestMultipleOf n: Self) -> Self {
    precondition(n != 0)
    let m = Self(n.magnitude)
    return (self < 0 ? self : self + (m - 1)) / m * m
  }

}

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
