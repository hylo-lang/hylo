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

  func testTupleComposeDecompose() throws {
    var l = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())
    let i16Pair = ^TupleType(types: [.builtin(.i(16)), .builtin(.i(16))])
    let i16 = ^BuiltinType.i(16)

    assert(l[i16Pair].alignment > 1, "Need to produce misaligned access for testing")

    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(i16Pair).address

    check(throws: Memory.Error.alignment(p + 1, for: l[i16])) {
      try m.compose(i16, at: p + 1)
    }

    // An address that would be suitably aligned, but out of bounds for
    // i16Pair initialization.  The initialized object would extend past
    // the end of the allocation.
    let outOfBoundsFori16Pair = p + l[i16].size
    check(
      throws: Memory.Error.bounds(outOfBoundsFori16Pair, for: l[i16Pair], allocationSize: l[i16Pair].size)
    ) {
      try m.compose(i16Pair, at: outOfBoundsFori16Pair)
    }

    let parts = l[i16Pair].parts
    let partIDs = Array(l[i16Pair].partParentages)
    check(throws: Memory.Error.noComposedPart(at: p, partIDs[0])) {
      try m.compose(i16Pair, at: p)
    }

    try m.compose(i16, at: p + parts[0].offset)

    check(throws: Memory.Error.noComposedPart(at: p + parts[1].offset, partIDs[1])) {
      try m.compose(i16Pair, at: p)
    }

    try m.compose(i16, at: p + parts[1].offset)

    try m.compose(i16Pair, at: p)

    check(throws: Memory.Error.noDecomposable(l[i16], at: p)) {
      try m.decompose(i16, at: p)
    }

    try m.decompose(i16Pair, at: p)

    check(throws:Memory.Error.noDecomposable(l[i16Pair], at: p)) {
      try m.decompose(i16Pair, at: p)
    }

    try m.decompose(i16, at: p + parts[0].offset)

    check(throws: Memory.Error.noDecomposable(l[i16], at: p + parts[0].offset)) {
      try m.decompose(i16, at: p)
    }

    try m.decompose(i16, at: p + parts[1].offset)
  }

  func testUnionComposeDecompose() throws {
    var l = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())
    let i16 = ^BuiltinType.i(16)
    let i32 = ^BuiltinType.i(32)
    let i16i32Union = ^UnionType([i16, i32])

    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(i16i32Union).address

    assert(l[i16i32Union].alignment > 1)

    check(throws: Memory.Error.alignment(p + 1, for: l[i16i32Union])) {
      try m.compose(i16i32Union, at: p + 1)
    }

    // An address that would be suitably aligned, but out of bounds for
    // i16i32Union initialization.  The initialized object would extend past
    // the end of the allocation.
    let outOfBoundsFori16i32Union = p + l[i32].size
    check(
      throws: Memory.Error.bounds(outOfBoundsFori16i32Union, for: l[i16i32Union], allocationSize: l[i16i32Union].size)
    ) {
      try m.compose(i16i32Union, at: outOfBoundsFori16i32Union)
    }

    let parts = l[i16i32Union].parts
    let partIDs = Array(l[i16i32Union].partParentages)
    let discriminator = parts.last!
    check(throws: Memory.Error.noComposedPart(at: p + discriminator.offset, partIDs.last!)) {
      try m.compose(i16i32Union, at: p)
    }
    _ = i16
    /*
      m.compose()
      try m.compose(i16, at: p + parts[0].offset)

      XCTAssertThrowsError(Memory.Error.noComposedPart(at: p + parts[1].offset, partIDs[1])) {
        try m.compose(i16i32Union, at: p)
      }

      try m.compose(i16, at: p + parts[1].offset)

      try m.compose(i16i32Union, at: p)

      XCTAssertThrowsError(Memory.Error.noDecomposable(i16, at: p)) {
        try m.decompose(i16, at: p)
      }

      try m.decompose(i16i32Union, at: p)

      XCTAssertThrowsError(Memory.Error.noDecomposable(i16i32Union, at: p)) {
        try m.decompose(i16i32Union, at: p)
      }

      try m.decompose(i16, at: p + parts[0].offset)

      XCTAssertThrowsError(Memory.Error.noDecomposable(i16, at: p + parts[0].offset)) {
        try m.decompose(i16, at: p)
      }

      try m.decompose(i16, at: p + parts[1].offset)
     */
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
