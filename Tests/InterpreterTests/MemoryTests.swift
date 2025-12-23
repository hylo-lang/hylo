import FrontEnd
import XCTest
import TestUtils
import Interpreter

final class InterpreterMemoryTests: XCTestCase {

  let emptyProgram = makeEmptyProgram()

  func testAllocation() throws {
    var m = Memory()
    var allocations: [Memory.Address] = []
    for alignmentPower in 0..<8 {
      let alignment = 1 << alignmentPower
      let p = m.allocate(3, bytesWithAlignment: alignment)
      allocations.append(p)
      XCTAssertEqual(m.allocation[p.allocation]!.size, 3, "alignment \(alignment)")
      XCTAssert(m.address(p, hasAlignment: alignment))
    }

    for p in allocations {
      try m.deallocate(p)

      check(throws: Memory.Error.noLongerAllocated(p)) { try m.deallocate(p) }

      let q = Memory.Address(allocation: p.allocation, offset: p.offset + 1)
      check(throws: Memory.Error.deallocationNotAtStartOfAllocation(q)) { try m.deallocate(q) }
    }
  }

  func testTupleComposeDecompose() throws {
    var layouts = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())
    let i16Pair = layouts[^TupleType(types: [.builtin(.i(16)), .builtin(.i(16))])]
    let i16 = layouts[.builtin(.i(16))]

    assert(i16Pair.alignment > 1, "Need to produce misaligned access for testing")

    var m = Memory()
    let p = m.allocate(i16Pair.size, bytesWithAlignment: i16Pair.alignment)

    check(throws: Memory.Error.alignment(p + 1, for: i16)) {
      try m.compose(i16, at: p + 1)
    }

    // An address that would be suitably aligned, but out of bounds for
    // i16Pair initialization.  The initialized object would extend past
    // the end of the allocation.
    let outOfBoundsFori16Pair = p + i16.size
    check(
      throws: Memory.Error.bounds(outOfBoundsFori16Pair, for: i16Pair, allocationSize: i16Pair.size)
    ) {
      try m.compose(i16Pair, at: outOfBoundsFori16Pair)
    }

    let parts = i16Pair.parts
    let partIDs = Array(i16Pair.partParentages)
    check(throws: Memory.Error.noComposedPart(at: p, partIDs[0])) {
      try m.compose(i16Pair, at: p)
    }

    try m.compose(i16, at: p + parts[0].offset)

    check(throws: Memory.Error.noComposedPart(at: p + parts[1].offset, partIDs[1])) {
      try m.compose(i16Pair, at: p)
    }

    try m.compose(i16, at: p + parts[1].offset)

    try m.compose(i16Pair, at: p)

    check(throws: Memory.Error.noDecomposable(i16, at: p)) {
      try m.decompose(i16, at: p)
    }

    try m.decompose(i16Pair, at: p)

    check(throws:Memory.Error.noDecomposable(i16Pair, at: p)) {
      try m.decompose(i16Pair, at: p)
    }

    try m.decompose(i16, at: p + parts[0].offset)

    check(throws: Memory.Error.noDecomposable(i16, at: p + parts[0].offset)) {
      try m.decompose(i16, at: p)
    }

    try m.decompose(i16, at: p + parts[1].offset)
  }

  func testUnionComposeDecompose() throws {
    var layouts = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())
    let i16i32Union = layouts[^UnionType([.builtin(.i(16)), .builtin(.i(32))])]
    let i16 = layouts[.builtin(.i(16))]
    let i32 = layouts[.builtin(.i(32))]

    var m = Memory()
    let p = m.allocate(i16i32Union.size, bytesWithAlignment: i16i32Union.alignment)

    assert(i16i32Union.alignment > 1)

    check(throws: Memory.Error.alignment(p + 1, for: i16i32Union)) {
      try m.compose(i16i32Union, at: p + 1)
    }

    // An address that would be suitably aligned, but out of bounds for
    // i16i32Union initialization.  The initialized object would extend past
    // the end of the allocation.
    let outOfBoundsFori16i32Union = p + i32.size
    check(
      throws: Memory.Error.bounds(
        outOfBoundsFori16i32Union, for: i16i32Union, allocationSize: i16i32Union.size)
    ) {
      try m.compose(i16i32Union, at: outOfBoundsFori16i32Union)
    }

    let parts = i16i32Union.parts
    let partIDs = Array(i16i32Union.partParentages)
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

  func testCheckingAllocationContainsAType() {
    var m = Memory()
    var c = TypeLayoutCache(typesIn: emptyProgram, for: UnrealABI())
    let inner = ^TupleType(types: [^BuiltinType.i(1)])
    let outer = ^TupleType(types: [^BuiltinType.i(1), inner])
    let a = m.allocate(c[outer])
    XCTAssertTrue(m[a.allocation].contains(outer, at: 0, with: &c))
    XCTAssertTrue(m[a.allocation].contains(^BuiltinType.i(1), at: 0, with: &c))
    XCTAssertTrue(m[a.allocation].contains(inner, at: 1, with: &c))
    XCTAssertTrue(m[a.allocation].contains(^BuiltinType.i(1), at: 1, with: &c))
    XCTAssertFalse(m[a.allocation].contains(inner, at: 0, with: &c))
    XCTAssertFalse(m[a.allocation].contains(outer, at: 1, with: &c))
    XCTAssertFalse(m[a.allocation].contains(inner, at: 2, with: &c))

    // TODO: tests for union case. Need to support open_union instruction for the same.
  }
}
