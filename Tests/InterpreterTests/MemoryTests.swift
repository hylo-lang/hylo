import FrontEnd
import XCTest
import TestUtils
import Interpreter

final class InterpreterMemoryTests: XCTestCase {

  let emptyProgram = makeEmptyProgram()

  func testAllocation() throws {
    var l = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())
    var m = Memory(l)
    var allocations: [Memory.Address] = []
    for sizeInBits in [8, 16, 32, 64, 128] {
      let t = BuiltinType.i(sizeInBits)
      let alignment = l[^t].alignment
      let p = m.allocate(^t)
      allocations.append(p)
      XCTAssertEqual(m.allocation[p.allocation]!.size, sizeInBits / 8, "alignment \(alignment)")
      XCTAssert(m.address(p, hasAlignment: alignment))
    }

    for p in allocations {
      try m.deallocate(p)

      check(throws: Memory.Error.noLongerAllocated(p)) { try m.deallocate(p) }

      let q = p.after(1, bytesHavingType: p.type)
      check(throws: Memory.Error.deallocationNotAtStartOfAllocation(q)) { try m.deallocate(q) }
    }
  }

  func testTupleComposeDecompose() throws {
    var l = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())
    let i16Pair = ^TupleType(types: [.builtin(.i(16)), .builtin(.i(16))])
    let i16 = ^BuiltinType.i(16)

    assert(l[i16Pair].alignment > 1, "Need to produce misaligned access for testing")

    var m = Memory(l)
    let p = m.allocate(i16Pair)

    check(throws: Memory.Error.alignment(p.after(1, bytesHavingType: p.type), for: l[i16])) {
      try m.compose(p.after(1, bytesHavingType: p.type))
    }

    // An address that would be suitably aligned, but out of bounds for
    // i16Pair initialization.  The initialized object would extend past
    // the end of the allocation.
    let outOfBoundsFori16Pair = p.after(l[i16].size, bytesHavingType: i16)
    check(
      throws: Memory.Error.bounds(
        outOfBoundsFori16Pair, for: l[i16Pair], allocationSize: l[i16Pair].size)
    ) {
      try m.compose(outOfBoundsFori16Pair)
    }

    let parts = l[i16Pair].parts
    let partIDs = Array(l[i16Pair].partParentages)
    check(throws: Memory.Error.noComposedPart(at: p, partIDs[0])) {
      try m.compose(p)
    }

    try m.compose(p.after(parts[0].offset, bytesHavingType: i16))

    check(
      throws: Memory.Error.noComposedPart(
        at: p.after(parts[1].offset, bytesHavingType: i16Pair), partIDs[1]
      )
    ) {
      try m.compose(p)
    }

    try m.compose(p.after(parts[1].offset, bytesHavingType: i16))
    try m.compose(p)

    check(throws: Memory.Error.noDecomposable(l[i16], at: p)) {
      try m.decompose(p.withType(i16))
    }

    try m.decompose(p)

    check(throws: Memory.Error.noDecomposable(l[i16Pair], at: p)) {
      try m.decompose(p)
    }

    try m.decompose(p.after(parts[0].offset, bytesHavingType: i16))

    check(
      throws: Memory.Error.noDecomposable(
        l[i16], at: p.after(parts[0].offset, bytesHavingType: i16)
      )
    ) {
      try m.decompose(p.withType(i16))
    }

    try m.decompose(p.after(parts[1].offset, bytesHavingType: i16))
  }

  func testUnionComposeDecompose() throws {
    var l = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())
    let i16i32Union = ^UnionType([.builtin(.i(16)), .builtin(.i(32))])
    let i16 = ^BuiltinType.i(16)
    let i32 = ^BuiltinType.i(32)

    var m = Memory(l)
    let p = m.allocate(i16i32Union)

    assert(l[i16i32Union].alignment > 1)

    check(
      throws: Memory.Error.alignment(p.after(1, bytesHavingType: i16i32Union), for: l[i16i32Union])
    ) {
      try m.compose(p.after(1, bytesHavingType: i16i32Union))
    }

    // An address that would be suitably aligned, but out of bounds for
    // i16i32Union initialization.  The initialized object would extend past
    // the end of the allocation.
    let outOfBoundsFori16i32Union = p.after(l[i32].size, bytesHavingType: i16i32Union)
    check(
      throws: Memory.Error.bounds(
        outOfBoundsFori16i32Union, for: l[i16i32Union], allocationSize: l[i16i32Union].size)
    ) {
      try m.compose(outOfBoundsFori16i32Union)
    }

    let parts = l[i16i32Union].parts
    let partIDs = Array(l[i16i32Union].partParentages)
    let discriminator = parts.last!
    check(
      throws: Memory.Error.noComposedPart(
        at: p.after(discriminator.offset, bytesHavingType: p.type), partIDs.last!)
    ) {
      try m.compose(p)
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

  func testSubFieldLayout() throws {
    var m = Memory(TypeLayoutCache(typesIn: emptyProgram, for: UnrealABI()))
    let t = ^TupleType(types: [
      ^BuiltinType.i(32), ^TupleType(types: [^BuiltinType.i(32), ^BuiltinType.i(8)]),
    ])
    let a = m.allocate(t)
    let x = m.address(of: [1, 1], in: a)
    XCTAssertEqual(x, .init(allocation: a.allocation, offset: 8, type: ^BuiltinType.i(8)))
    // TODO: add test for union case.
  }
}
