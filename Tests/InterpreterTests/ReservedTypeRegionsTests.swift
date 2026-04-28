import FrontEnd
import TestUtils
import XCTest

@testable import Interpreter

final class ReservedTypeRegionsTests: XCTestCase {

  func testTypeBindUnbind() throws {
    var l = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())
    let i64 = ^BuiltinType.i(64)
    let tuple = ^TupleType(types: [i64, i64])
    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(i64, count: 8).address

    var r = ReservedTypeRegions(
      allocation: withUnsafePointer(to: m.allocation[p.allocation]!) { $0 },
      typeLayouts: withUnsafeMutablePointer(to: &l) { $0 })

    check(throws: Memory.Error.alignment(p + 1, for: l[i64])) {
      try r.bind(i64, at: 1)
    }
    check(throws: Memory.Error.bounds(p + 64, for: l[i64], allocationSize: l[i64].size * 8)) {
      try r.bind(i64, at: 64)
    }

    try r.bind(tuple, at: 0)
    check(throws: ReservedTypeRegions.Error.regionAlreadyBound(to: tuple)) {
      try r.bind(i64, at: 0)
    }
    let e = Memory.Allocation.TypedRegion(startOffset: 0, type: tuple)
    XCTAssertEqual(r.region(enclosing: 0), e)
    XCTAssertEqual(r.region(enclosing: 4), e)
    XCTAssertEqual(r.region(enclosing: 16), nil)
    XCTAssertEqual(r.region(enclosing: 32), nil)
    r.unbind(at: 0)
    XCTAssertEqual(r.region(enclosing: 0), nil)
    XCTAssertEqual(r.region(enclosing: 4), nil)
    try r.bind(tuple, at: 0)
    try r.bind(tuple, at: 16)
  }

  // TODO: test regionEnclosing thing

}
