import FrontEnd
import XCTest
import TestUtils
import Interpreter

final class InterpreterMemoryTests: XCTestCase {

  let emptyProgram = makeEmptyProgram()

  func testAllocation() throws {
    var l = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())
    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    var allocations: [Memory.Place] = []
    for sizeInBits in [8, 16, 32, 64, 128] {
      let t = BuiltinType.i(sizeInBits)
      let alignment = l[^t].alignment
      let p = m.allocate(^t)
      allocations.append(p)
      XCTAssertEqual(m.allocation[p.allocation]!.size, sizeInBits / 8, "alignment \(alignment)")
      XCTAssert(m.place(p, hasAlignment: alignment))
    }

    for p in allocations {
      try m.deallocate(p)

      check(throws: Memory.Error.noLongerAllocated(p.address)) { try m.deallocate(p) }

      let q = (p.address + 1).asPlace(of: p.type)
      check(throws: Memory.Error.deallocationNotAtStartOfAllocation(q.address)) { try m.deallocate(q) }
    }
  }

  func testSubPartLayout() throws {
    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let i8 = ^BuiltinType.i(8);
    let i32 = ^BuiltinType.i(32);
    let inner = ^TupleType(types: [i32, i8])
    let t = ^TupleType(types: [i32, inner])
    let a = m.allocate(t)
    XCTAssertEqual(m.location(of: [], in: a), a)
    XCTAssertEqual(
      m.location(of: [0], in: a),
      .init(allocation: a.allocation, offset: 0, type: i32))
    XCTAssertEqual(
      m.location(of: [1], in: a),
      .init(allocation: a.allocation, offset: 4, type: inner))
    XCTAssertEqual(
      m.location(of: [1, 0], in: a),
      .init(allocation: a.allocation, offset: 4, type: i32))
    XCTAssertEqual(
      m.location(of: [1, 1], in: a),
      .init(allocation: a.allocation, offset: 8, type: i8))
    // TODO: add test for union case.
  }

}
