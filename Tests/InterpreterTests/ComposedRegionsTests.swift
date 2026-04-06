import FrontEnd
import TestUtils
import XCTest

@testable import Interpreter

final class ComposedRegionsTests: XCTestCase {

  func test_requireInitialized() throws {
    var layouts = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())
    let void_ = AnyType.void
    let voidPair = ^TupleType(types: [.void, .void])

    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(voidPair).address

    var c = ComposedRegions(
      for: withUnsafePointer(to: m.allocation[p.allocation]!) { $0 },
      havingLayoutsFrom: withUnsafeMutablePointer(to: &layouts) { $0 })

    let voidPairPart0 = layouts[voidPair].partParentages.first!
    check(throws: Memory.Error.noComposedPart(at: p, voidPairPart0)) {
      try c.requireComposed(part: voidPairPart0, baseOffset: 0, region: 0)
    }

    try c.compose(void_, at: p.offset + layouts[voidPair].parts[0].offset)
    // It should be possible to initialize both parts at the same address
    try c.compose(void_, at: p.offset + layouts[voidPair].parts[1].offset)
    try c.compose(voidPair, at: p.offset)

    let i8Pair = layouts[^TupleType(types: [.builtin(.i(8)), .builtin(.i(8))])]
    let i8PairPart0 = i8Pair.partParentages.first!

    check(throws: Memory.Error.partType(voidPair, part: i8PairPart0)) {
      try c.requireComposed(part: i8PairPart0, baseOffset: 0, region: 0)
    }
  }

  func testTupleComposeDecompose() throws {
    var l = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())
    let i16Pair = ^TupleType(types: [.builtin(.i(16)), .builtin(.i(16))])
    let i16 = ^BuiltinType.i(16)

    assert(l[i16Pair].alignment > 1, "Need to produce misaligned access for testing")

    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(i16Pair).address
    var c = ComposedRegions(
      for: withUnsafePointer(to: m.allocation[p.allocation]!) { $0 },
      havingLayoutsFrom: withUnsafeMutablePointer(to: &l) { $0 })

    check(throws: Memory.Error.alignment(p + 1, for: l[i16])) {
      try c.compose(i16, at: p.offset + 1)
    }

    // An address that would be suitably aligned, but out of bounds for
    // i16Pair initialization.  The initialized object would extend past
    // the end of the allocation.
    let outOfBoundsFori16Pair = p + l[i16].size
    check(
      throws: Memory.Error.bounds(
        outOfBoundsFori16Pair, for: l[i16Pair], allocationSize: l[i16Pair].size)
    ) {
      try c.compose(i16Pair, at: outOfBoundsFori16Pair.offset)
    }

    let parts = l[i16Pair].parts
    let partIDs = Array(l[i16Pair].partParentages)
    check(throws: Memory.Error.noComposedPart(at: p, partIDs[0])) {
      try c.compose(i16Pair, at: p.offset)
    }

    try c.compose(i16, at: p.offset + parts[0].offset)

    check(throws: Memory.Error.noComposedPart(at: p + parts[1].offset, partIDs[1])) {
      try c.compose(i16Pair, at: p.offset)
    }

    try c.compose(i16, at: p.offset + parts[1].offset)

    try c.compose(i16Pair, at: p.offset)

    check(throws: Memory.Error.noDecomposable(l[i16], at: p)) {
      try c.decompose(i16, at: p.offset)
    }

    try c.decompose(i16Pair, at: p.offset)

    check(throws: Memory.Error.noDecomposable(l[i16Pair], at: p)) {
      try c.decompose(i16Pair, at: p.offset)
    }

    try c.decompose(i16, at: p.offset + parts[0].offset)

    check(throws: Memory.Error.noDecomposable(l[i16], at: p + parts[0].offset)) {
      try c.decompose(i16, at: p.offset)
    }

    try c.decompose(i16, at: p.offset + parts[1].offset)
  }

  func testUnionComposeDecompose() throws {
    var l = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())
    let i16 = ^BuiltinType.i(16)
    let i32 = ^BuiltinType.i(32)
    let i16i32Union = ^UnionType([i16, i32])

    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(i16i32Union).address
    var c = ComposedRegions(
      for: withUnsafePointer(to: m.allocation[p.allocation]!) { $0 },
      havingLayoutsFrom: withUnsafeMutablePointer(to: &l) { $0 })

    assert(l[i16i32Union].alignment > 1)

    check(throws: Memory.Error.alignment(p + 1, for: l[i16i32Union])) {
      try c.compose(i16i32Union, at: p.offset + 1)
    }

    // An address that would be suitably aligned, but out of bounds for
    // i16i32Union initialization.  The initialized object would extend past
    // the end of the allocation.
    let outOfBoundsFori16i32Union = p + l[i32].size
    check(
      throws: Memory.Error.bounds(
        outOfBoundsFori16i32Union, for: l[i16i32Union], allocationSize: l[i16i32Union].size)
    ) {
      try c.compose(i16i32Union, at: outOfBoundsFori16i32Union.offset)
    }

    let parts = l[i16i32Union].parts
    let partIDs = Array(l[i16i32Union].partParentages)
    let discriminator = parts.last!
    check(throws: Memory.Error.noComposedPart(at: p + discriminator.offset, partIDs.last!)) {
      try c.compose(i16i32Union, at: p.offset)
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
}
