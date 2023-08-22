import Utils
import XCTest

final class MathTests: XCTestCase {

  func testFactorial() {
    XCTAssertEqual(0.factorial, 1)
    XCTAssertEqual(1.factorial, 1)
    XCTAssertEqual(2.factorial, 2)
    XCTAssertEqual(3.factorial, 6)
    XCTAssertEqual(4.factorial, 24)
  }

}
