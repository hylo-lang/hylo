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
    let a = p.allocation

    var r = ReservedTypeRegions()

    check(throws: Memory.Error.alignment(p + 1, for: l[i64])) {
      try r.bind(i64, at: 1, in: m[a], typeLayouts: &l)
    }
    check(throws: Memory.Error.bounds(p + 64, for: l[i64], allocationSize: l[i64].size * 8)) {
      try r.bind(i64, at: 64, in: m[a], typeLayouts: &l)
    }

    try r.bind(tuple, at: 0, in: m[a], typeLayouts: &l)
    check(throws: ReservedTypeRegions.Error.regionAlreadyBound(to: tuple)) {
      try r.bind(i64, at: 0, in: m[a], typeLayouts: &l)
    }
    let e = Memory.Allocation.TypedRegion(offset: 0, type: tuple)
    XCTAssertEqual(r.region(enclosing: 0, typeLayouts: &l), e)
    XCTAssertEqual(r.region(enclosing: 4, typeLayouts: &l), e)
    XCTAssertEqual(r.region(enclosing: 16, typeLayouts: &l), nil)
    XCTAssertEqual(r.region(enclosing: 32, typeLayouts: &l), nil)
    r.unbind(at: 0)
    XCTAssertEqual(r.region(enclosing: 0, typeLayouts: &l), nil)
    XCTAssertEqual(r.region(enclosing: 4, typeLayouts: &l), nil)
    try r.bind(tuple, at: 0, in: m[a], typeLayouts: &l)
    try r.bind(tuple, at: 16, in: m[a], typeLayouts: &l)
  }

}
