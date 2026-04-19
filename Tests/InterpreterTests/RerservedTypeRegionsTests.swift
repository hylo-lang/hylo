import FrontEnd
import TestUtils
import XCTest

@testable import Interpreter

final class ReservedTypeRegionsTests: XCTestCase {

  func testTypeBindUnbind() throws {
    var l = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())
    let i64 = ^BuiltinType.i(64)
    let tuple = ^TupleType(types: [i64, i64])

    var r = ReservedTypeRegions(havingLayoutsFrom: withUnsafeMutablePointer(to: &l) { $0 })

    try r.reserve(tuple, at: 0)
    check(throws: Memory.Error.regionAlreadyReserved(for: tuple)) {
      try r.reserve(i64, at: 0)
    }
    r.removeTypeReservation(from: 0)
    try r.reserve(tuple, at: 0)
    try r.reserve(tuple, at: 16)
  }

  // TODO: test regionEnclosing thing

}
