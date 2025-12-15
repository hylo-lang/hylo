import XCTest

@testable import Interpreter

final class BuiltinValueTests: XCTestCase {

  func testIntegerAccessors() {
    var x = BuiltinValue.i1(true)
    XCTAssertEqual(x.i1, true)
    XCTAssertEqual(x.i8, nil)
    XCTAssertEqual(x.i16, nil)
    XCTAssertEqual(x.i32, nil)
    XCTAssertEqual(x.i64, nil)
    XCTAssertEqual(x.i128, nil)

    x = BuiltinValue.i8(2)
    XCTAssertEqual(x.i1, nil)
    XCTAssertEqual(x.i8, 2)
    XCTAssertEqual(x.i16, nil)
    XCTAssertEqual(x.i32, nil)
    XCTAssertEqual(x.i64, nil)
    XCTAssertEqual(x.i128, nil)

    x = BuiltinValue.i16(2)
    XCTAssertEqual(x.i1, nil)
    XCTAssertEqual(x.i8, nil)
    XCTAssertEqual(x.i16, 2)
    XCTAssertEqual(x.i32, nil)
    XCTAssertEqual(x.i64, nil)
    XCTAssertEqual(x.i128, nil)

    x = BuiltinValue.i32(2)
    XCTAssertEqual(x.i1, nil)
    XCTAssertEqual(x.i8, nil)
    XCTAssertEqual(x.i16, nil)
    XCTAssertEqual(x.i32, 2)
    XCTAssertEqual(x.i64, nil)
    XCTAssertEqual(x.i128, nil)

    x = BuiltinValue.i64(2)
    XCTAssertEqual(x.i1, nil)
    XCTAssertEqual(x.i8, nil)
    XCTAssertEqual(x.i16, nil)
    XCTAssertEqual(x.i32, nil)
    XCTAssertEqual(x.i64, 2)
    XCTAssertEqual(x.i128, nil)

    x = BuiltinValue.i128(2)
    XCTAssertEqual(x.i1, nil)
    XCTAssertEqual(x.i8, nil)
    XCTAssertEqual(x.i16, nil)
    XCTAssertEqual(x.i32, nil)
    XCTAssertEqual(x.i64, nil)
    XCTAssertEqual(x.i128, 2)
  }

}
