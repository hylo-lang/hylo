import IR
import XCTest

final class Base64VarUIntTests: XCTestCase {

  func testRoundTrip() {
    for i: UInt64 in 0 ..< 5000 {
      let s = Base64VarUInt(i).description
      XCTAssertEqual(Base64VarUInt(s)?.rawValue, i)
    }

    for i: UInt64 in 10000 ..< 15000 {
      let s = Base64VarUInt(i).description
      XCTAssertEqual(Base64VarUInt(s)?.rawValue, i)
    }

    for i: UInt64 in 100000 ..< 105000 {
      let s = Base64VarUInt(i).description
      XCTAssertEqual(Base64VarUInt(s)?.rawValue, i)
    }
  }

}
