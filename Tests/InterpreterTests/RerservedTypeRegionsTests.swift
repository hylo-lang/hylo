import FrontEnd
import TestUtils
import XCTest

@testable import Interpreter

final class ReservedTypeRegionsTests: XCTestCase {

  func testTypeBindUnbind() throws {
    var l = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())
    let i64 = ^BuiltinType.i(64)
    let tuple = ^TupleType(types: [i64, i64])

    var r = ReservedTypeRegions(typeLayouts: withUnsafeMutablePointer(to: &l) { $0 })

    try r.bind(tuple, at: 0)
    check(throws: Memory.Error.regionAlreadyReserved(for: tuple)) {
      try r.bind(i64, at: 0)
    }
    let e = Memory.Allocation.TypedRegion(startOffset: 0, type: tuple)
    XCTAssertEqual(r.region(enclosing: 0), e)
    XCTAssertEqual(r.region(enclosing: 4), e)
    XCTAssertEqual(r.region(enclosing: 16), nil)
    XCTAssertEqual(r.region(enclosing: 32), nil)
    r.unbind(at: 0)
    try r.bind(tuple, at: 0)
    try r.bind(tuple, at: 16)
  }

  // TODO: test regionEnclosing thing

}
