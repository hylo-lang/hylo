import Utils
import XCTest

final class ThreeValuedBitTests: XCTestCase {

  private typealias T = ThreeValuedBit

  func testNot() {
    XCTAssertEqual(!T(true), false)
    XCTAssertEqual(!T(false), true)
    XCTAssertEqual(!T(nil), nil)
  }

  func testAnd() {
    XCTAssertEqual(T(true) && T(true), true)
    XCTAssertEqual(T(true) && T(false), false)
    XCTAssertEqual(T(true) && T(nil), nil)

    XCTAssertEqual(T(false) && T(true), false)
    XCTAssertEqual(T(false) && T(false), false)
    XCTAssertEqual(T(false) && T(nil), false)

    XCTAssertEqual(T(nil) && T(true), nil)
    XCTAssertEqual(T(nil) && T(false), false)
    XCTAssertEqual(T(nil) && T(nil), nil)
  }

  func testOr() {
    XCTAssertEqual(T(true) || T(true), true)
    XCTAssertEqual(T(true) || T(false), true)
    XCTAssertEqual(T(true) || T(nil), true)

    XCTAssertEqual(T(false) || T(true), true)
    XCTAssertEqual(T(false) || T(false), false)
    XCTAssertEqual(T(false) || T(nil), nil)

    XCTAssertEqual(T(nil) || T(true), true)
    XCTAssertEqual(T(nil) || T(false), nil)
    XCTAssertEqual(T(nil) || T(nil), nil)
  }

}
