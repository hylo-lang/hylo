import FrontEnd
import TestUtils
import XCTest

@testable import Interpreter

final class ComposedRegionsTests: XCTestCase {

  func test_requireInitialized() {
    var layouts = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())
    let void_ = AnyType.void
    let voidPair = ^TupleType(types: [.void, .void])

    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(voidPair).address

    var c = ComposedRegions(
      allocation: withUnsafePointer(to: m.allocation[p.allocation]!) { $0 },
      typeLayouts: withUnsafeMutablePointer(to: &layouts) { $0 })

    XCTAssertFalse(c.canCompose(voidPair, at: 0))

    // It should be possible to initialize both parts at the same address
    XCTAssertTrue(c.canCompose(void_, at: 0))
    c.compose(void_, at: p.offset + layouts[voidPair].parts[0].offset)
    XCTAssertTrue(c.canCompose(void_, at: 0))
    c.compose(void_, at: p.offset + layouts[voidPair].parts[1].offset)
    XCTAssertTrue(c.canCompose(voidPair, at: 0))
    c.compose(voidPair, at: p.offset)
    XCTAssertTrue(c.canCompose(^BuiltinType.i(8), at: 0))
  }

  func testTupleComposeDecompose() {
    var l = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())
    let i16Pair = ^TupleType(types: [.builtin(.i(16)), .builtin(.i(16))])
    let i16 = ^BuiltinType.i(16)

    assert(l[i16Pair].alignment > 1, "Need to produce misaligned access for testing")

    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(i16Pair).address
    var c = ComposedRegions(
      allocation: withUnsafePointer(to: m.allocation[p.allocation]!) { $0 },
      typeLayouts: withUnsafeMutablePointer(to: &l) { $0 })

    let parts = l[i16Pair].parts
    XCTAssertFalse(c.canCompose(i16Pair, at: p.offset))

    c.compose(i16, at: p.offset + parts[0].offset)
    XCTAssertFalse(c.canCompose(i16Pair, at: p.offset))
    c.compose(i16, at: p.offset + parts[1].offset)
    XCTAssertTrue(c.canCompose(i16Pair, at: p.offset))
    c.compose(i16Pair, at: p.offset)

    XCTAssertFalse(c.tryDecompose(i16, at: p.offset))
    XCTAssertTrue(c.tryDecompose(i16Pair, at: p.offset))
    XCTAssertFalse(c.tryDecompose(i16Pair, at: p.offset))
    XCTAssertTrue(c.tryDecompose(i16, at: p.offset + parts[0].offset))
    XCTAssertFalse(c.tryDecompose(i16, at: p.offset))
    XCTAssertTrue(c.tryDecompose(i16, at: p.offset + parts[1].offset))
  }

  func testUnionComposeDecompose() {
    var l = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())
    let i16 = ^BuiltinType.i(16)
    let i32 = ^BuiltinType.i(32)
    let i16i32Union = ^UnionType([i16, i32])

    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(i16i32Union).address
    let c = ComposedRegions(
      allocation: withUnsafePointer(to: m.allocation[p.allocation]!) { $0 },
      typeLayouts: withUnsafeMutablePointer(to: &l) { $0 })

    assert(l[i16i32Union].alignment > 1)

    XCTAssertFalse(c.canCompose(i16i32Union, at: p.offset))
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

  func testIsComplete() {
    var l = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())

    let i8 = ^BuiltinType.i(8)
    let i8Pair = ^TupleType(types: [i8, i8])
    let t = ^TupleType(types: [i8, i8Pair])

    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(i8, count: 132).address
    let e = Memory.Place(allocation: p.allocation, offset: l[t].parts[1].offset, type: i8Pair)
    var c = ComposedRegions(
      allocation: withUnsafePointer(to: m.allocation[p.allocation]!) { $0 },
      typeLayouts: withUnsafeMutablePointer(to: &l) { $0 }
    )
    XCTAssertFalse(c.isComplete(e))
    c.compose(i8, at: l[t].parts[0].offset)
    XCTAssertFalse(c.isComplete(e))
    c.compose(i8, at: l[t].parts[1].offset + l[i8Pair].parts[0].offset)
    XCTAssertFalse(c.isComplete(e))
    c.compose(i8, at: l[t].parts[1].offset + l[i8Pair].parts[1].offset)
    XCTAssertFalse(c.isComplete(e))
    c.compose(i8Pair, at: l[t].parts[1].offset)
    XCTAssertTrue(c.isComplete(e))
    c.compose(t, at: 0)
    XCTAssertTrue(c.isComplete(e))
  }

  func testIsFullyUninitialized() {
    var l = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())

    let i8 = ^BuiltinType.i(8)
    let i8Pair = ^TupleType(types: [i8, i8])
    let t = ^TupleType(types: [i8, i8Pair])

    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(i8, count: 132).address
    let e = Memory.Place(allocation: p.allocation, offset: l[t].parts[1].offset, type: i8Pair)
    var c = ComposedRegions(
      allocation: withUnsafePointer(to: m.allocation[p.allocation]!) { $0 },
      typeLayouts: withUnsafeMutablePointer(to: &l) { $0 }
    )

    XCTAssertTrue(c.isFullyUninitialized(e))
    c.compose(i8, at: l[t].parts[0].offset)
    XCTAssertTrue(c.isFullyUninitialized(e))
    c.compose(i8, at: l[t].parts[1].offset + l[i8Pair].parts[1].offset)
    XCTAssertFalse(c.isFullyUninitialized(e))
    c.compose(i8, at: l[t].parts[1].offset + l[i8Pair].parts[0].offset)
    XCTAssertFalse(c.isFullyUninitialized(e))
    c.compose(i8Pair, at: l[t].parts[1].offset)
    XCTAssertFalse(c.isFullyUninitialized(e))
    c.compose(t, at: 0)
    XCTAssertFalse(c.isFullyUninitialized(e))
  }

  func testDecomposeSubtree() {
    var l = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())

    let i8 = ^BuiltinType.i(8)
    let i8Pair = ^TupleType(types: [i8, i8])
    let t = ^TupleType(types: [i8, i8Pair])

    let a = l[t].alignment

    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(i8, count: 132).address
    let e = Memory.Place(allocation: p.allocation, offset: a, type: t)
    var c = ComposedRegions(
      allocation: withUnsafePointer(to: m.allocation[p.allocation]!) { $0 },
      typeLayouts: withUnsafeMutablePointer(to: &l) { $0 }
    )

    c.compose(i8, at: 0)
    c.compose(i8, at: a + l[t].parts[0].offset)
    c.compose(i8, at: a + l[t].parts[1].offset + l[i8Pair].parts[0].offset)
    c.compose(i8, at: a + l[t].parts[1].offset + l[i8Pair].parts[1].offset)
    c.compose(i8Pair, at: a + l[t].parts[1].offset)
    c.compose(t, at: a)
    c.compose(i8, at: a + l[t].size)

    c.decomposeSubtree(of: e)
    XCTAssertEqual(c.region(enclosing: 0), .init(offset: 0, type: i8))
    XCTAssertEqual(c.region(enclosing: a), nil)
    XCTAssertEqual(c.region(enclosing: a + l[t].parts[0].offset), nil)
    XCTAssertEqual(c.region(enclosing: a + l[t].parts[0].offset + l[i8Pair].parts[0].offset), nil)
    XCTAssertEqual(c.region(enclosing: a + l[t].parts[0].offset + l[i8Pair].parts[1].offset), nil)
    XCTAssertEqual(c.region(enclosing: a + l[t].size), .init(offset: a + l[t].size, type: i8))
  }

  func testComposeUpwards() {
    var l = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())

    let i8 = ^BuiltinType.i(8)

    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(i8, count: 132).address
    var c = ComposedRegions(
      allocation: withUnsafePointer(to: m.allocation[p.allocation]!) { $0 },
      typeLayouts: withUnsafeMutablePointer(to: &l) { $0 }
    )
    let t1 = ^TupleType(types: [i8, i8])
    let t2 = ^TupleType(types: [t1, i8])
    let t3 = ^TupleType(types: [t2, i8])

    c.compose(i8, at: l[t3].parts[0].offset + l[t2].parts[0].offset + l[t1].parts[0].offset)
    c.compose(i8, at: l[t3].parts[0].offset + l[t2].parts[0].offset + l[t1].parts[1].offset)
    c.compose(i8, at: l[t3].parts[0].offset + l[t2].parts[1].offset)
    c.compose(i8, at: l[t3].parts[1].offset)

    c.composeUpwards(along: [
      .init(startOffset: l[t3].parts[0].offset, type: t2),
      .init(startOffset: l[t3].parts[0].offset + l[t2].parts[0].offset, type: t1),
      .init(
        startOffset: l[t3].parts[0].offset + l[t2].parts[0].offset + l[t1].parts[1].offset, type: i8
      ),
    ])

    XCTAssertEqual(
      c.region(enclosing: l[t3].parts[0].offset), .init(offset: l[t3].parts[0].offset, type: t2))
    XCTAssertEqual(
      c.region(enclosing: l[t3].parts[1].offset), .init(offset: l[t3].parts[1].offset, type: i8))
  }
}
