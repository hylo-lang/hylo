import FrontEnd
import TestUtils
import XCTest

@testable import Interpreter

final class ComposedRegionsTests: XCTestCase {

  func testRequireInitialized() {
    var l = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())
    let void = AnyType.void
    let voidPair = ^TupleType(types: [.void, .void])

    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(voidPair).address
    let a = p.allocation

    var c = ComposedRegions()

    XCTAssertFalse(c.canCompose(voidPair, at: 0, in: m[a], typeLayouts: &l))

    // It should be possible to initialize both parts at the same address
    XCTAssertTrue(c.canCompose(void, at: 0, in: m[a], typeLayouts: &l))
    c.compose(void, at: p.offset + l[voidPair].parts[0].offset, typeLayouts: &l)
    XCTAssertTrue(c.canCompose(void, at: 0, in: m[a], typeLayouts: &l))
    c.compose(void, at: p.offset + l[voidPair].parts[1].offset, typeLayouts: &l)
    XCTAssertTrue(c.canCompose(voidPair, at: 0, in: m[a], typeLayouts: &l))
    c.compose(voidPair, at: p.offset, typeLayouts: &l)
    XCTAssertTrue(c.canCompose(^BuiltinType.i(8), at: 0, in: m[a], typeLayouts: &l))
  }

  func testTupleComposeDecompose() {
    var l = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())
    let i16Pair = ^TupleType(types: [.builtin(.i(16)), .builtin(.i(16))])
    let i16 = ^BuiltinType.i(16)

    assert(l[i16Pair].alignment > 1, "Need to produce misaligned access for testing")

    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(i16Pair).address
    let a = p.allocation
    var c = ComposedRegions()

    let parts = l[i16Pair].parts
    XCTAssertFalse(c.canCompose(i16Pair, at: p.offset, in: m[a], typeLayouts: &l))

    c.compose(i16, at: p.offset + parts[0].offset, typeLayouts: &l)
    XCTAssertFalse(c.canCompose(i16Pair, at: p.offset, in: m[a], typeLayouts: &l))
    c.compose(i16, at: p.offset + parts[1].offset, typeLayouts: &l)
    XCTAssertTrue(c.canCompose(i16Pair, at: p.offset, in: m[a], typeLayouts: &l))
    c.compose(i16Pair, at: p.offset, typeLayouts: &l)

    XCTAssertFalse(c.tryDecompose(i16, at: p.offset, in: m[a], typeLayouts: &l))
    XCTAssertTrue(c.tryDecompose(i16Pair, at: p.offset, in: m[a], typeLayouts: &l))
    XCTAssertFalse(c.tryDecompose(i16Pair, at: p.offset, in: m[a], typeLayouts: &l))
    XCTAssertTrue(
      c.tryDecompose(
        i16, at: p.offset + parts[0].offset, in: m[a],
        typeLayouts: &l))
    XCTAssertFalse(c.tryDecompose(i16, at: p.offset, in: m[a], typeLayouts: &l))
    XCTAssertTrue(
      c.tryDecompose(
        i16, at: p.offset + parts[1].offset, in: m[a],
        typeLayouts: &l))
  }

  func testUnionComposeDecompose() {
    var l = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())
    let i8 = ^BuiltinType.i(8)
    let i16 = ^BuiltinType.i(16)
    let i32 = ^BuiltinType.i(32)
    let i16i32Union = ^UnionType([i16, i32])
    let parts = l[i16i32Union].parts

    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(i16i32Union).address
    let a = p.allocation
    var c = ComposedRegions()

    assert(l[i16i32Union].alignment > 1)

    XCTAssertFalse(c.canCompose(i16i32Union, at: p.offset, in: m[a], typeLayouts: &l))

    c.compose(i16, at: parts[0].offset, typeLayouts: &l)
    XCTAssertFalse(c.canCompose(i16i32Union, at: 0, in: m[a], typeLayouts: &l))
    c.compose(i8, at: parts[2].offset, typeLayouts: &l)
    XCTAssertTrue(c.canCompose(i16i32Union, at: 0, in: m[a], typeLayouts: &l))
    c.compose(i16i32Union, at: 0, typeLayouts: &l)
    
    XCTAssertFalse(c.tryDecompose(i16, at: 0, in: m[a], typeLayouts: &l))
    XCTAssertTrue(c.tryDecompose(i16i32Union, at: 0, in: m[a], typeLayouts: &l))
    XCTAssertFalse(c.tryDecompose(i16i32Union, at: 0, in: m[a], typeLayouts: &l))
    XCTAssertTrue(c.tryDecompose(i16, at: parts[0].offset, in: m[a], typeLayouts: &l))
    XCTAssertFalse(c.tryDecompose(i16, at: parts[0].offset, in: m[a], typeLayouts: &l))
    XCTAssertTrue(c.tryDecompose(i8, at: parts[2].offset, in: m[a], typeLayouts: &l))
  }

  func testIsComplete() {
    var l = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())

    let i8 = ^BuiltinType.i(8)
    let i8Pair = ^TupleType(types: [i8, i8])
    let t = ^TupleType(types: [i8, i8Pair])

    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let _ = m.allocate(i8, count: 132).address
    let e = Memory.Allocation.TypedRegion(offset: l[t].parts[1].offset, type: i8Pair)
    var c = ComposedRegions()

    XCTAssertFalse(c.isComplete(e, typeLayouts: &l))
    c.compose(i8, at: l[t].parts[0].offset, typeLayouts: &l)
    XCTAssertFalse(c.isComplete(e, typeLayouts: &l))
    c.compose(i8, at: l[t].parts[1].offset + l[i8Pair].parts[0].offset, typeLayouts: &l)
    XCTAssertFalse(c.isComplete(e, typeLayouts: &l))
    c.compose(i8, at: l[t].parts[1].offset + l[i8Pair].parts[1].offset, typeLayouts: &l)
    XCTAssertFalse(c.isComplete(e, typeLayouts: &l))
    c.compose(i8Pair, at: l[t].parts[1].offset, typeLayouts: &l)
    XCTAssertTrue(c.isComplete(e, typeLayouts: &l))
    c.compose(t, at: 0, typeLayouts: &l)
    XCTAssertTrue(c.isComplete(e, typeLayouts: &l))
  }

  func testIsFullyUninitialized() {
    var l = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())

    let i8 = ^BuiltinType.i(8)
    let i8Pair = ^TupleType(types: [i8, i8])
    let t = ^TupleType(types: [i8, i8Pair])

    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let _ = m.allocate(i8, count: 132).address
    let e = Memory.Allocation.TypedRegion(offset: l[t].parts[1].offset, type: i8Pair)
    var c = ComposedRegions()

    XCTAssertTrue(c.isFullyUninitialized(e, typeLayouts: &l))
    c.compose(i8, at: l[t].parts[0].offset, typeLayouts: &l)
    XCTAssertTrue(c.isFullyUninitialized(e, typeLayouts: &l))
    c.compose(i8, at: l[t].parts[1].offset + l[i8Pair].parts[1].offset, typeLayouts: &l)
    XCTAssertFalse(c.isFullyUninitialized(e, typeLayouts: &l))
    c.compose(i8, at: l[t].parts[1].offset + l[i8Pair].parts[0].offset, typeLayouts: &l)
    XCTAssertFalse(c.isFullyUninitialized(e, typeLayouts: &l))
    c.compose(i8Pair, at: l[t].parts[1].offset, typeLayouts: &l)
    XCTAssertFalse(c.isFullyUninitialized(e, typeLayouts: &l))
    c.compose(t, at: 0, typeLayouts: &l)
    XCTAssertFalse(c.isFullyUninitialized(e, typeLayouts: &l))
  }

  func testDecomposeSubtree() {
    var l = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())

    let i8 = ^BuiltinType.i(8)
    let i8Pair = ^TupleType(types: [i8, i8])
    let t = ^TupleType(types: [i8, i8Pair])

    let x = l[t].alignment

    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let _ = m.allocate(i8, count: 132).address
    let e = Memory.Allocation.TypedRegion(offset: x, type: t)
    var c = ComposedRegions()

    c.compose(i8, at: 0, typeLayouts: &l)
    c.compose(i8, at: x + l[t].parts[0].offset, typeLayouts: &l)
    c.compose(i8, at: x + l[t].parts[1].offset + l[i8Pair].parts[0].offset, typeLayouts: &l)
    c.compose(i8, at: x + l[t].parts[1].offset + l[i8Pair].parts[1].offset, typeLayouts: &l)
    c.compose(i8Pair, at: x + l[t].parts[1].offset, typeLayouts: &l)
    c.compose(t, at: x, typeLayouts: &l)
    c.compose(i8, at: x + l[t].size, typeLayouts: &l)

    c.decomposeSubtree(of: e, typeLayouts: &l)
    XCTAssertEqual(c.region(enclosing: 0, typeLayouts: &l), .init(offset: 0, type: i8))
    XCTAssertEqual(c.region(enclosing: x, typeLayouts: &l), nil)
    XCTAssertEqual(c.region(enclosing: x + l[t].parts[0].offset, typeLayouts: &l), nil)
    XCTAssertEqual(
      c.region(
        enclosing: x + l[t].parts[0].offset + l[i8Pair].parts[0].offset,
        typeLayouts: &l), nil)
    XCTAssertEqual(
      c.region(
        enclosing: x + l[t].parts[0].offset + l[i8Pair].parts[1].offset,
        typeLayouts: &l), nil)
    XCTAssertEqual(
      c.region(enclosing: x + l[t].size, typeLayouts: &l), .init(offset: x + l[t].size, type: i8))
  }

  func testComposeUpwards() {
    var l = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())

    let i8 = ^BuiltinType.i(8)

    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(i8, count: 132).address
    var c = ComposedRegions()

    let t1 = ^TupleType(types: [i8, i8])
    let t2 = ^TupleType(types: [t1, i8])
    let t3 = ^TupleType(types: [t2, i8])

    c.compose(
      i8, at: l[t3].parts[0].offset + l[t2].parts[0].offset + l[t1].parts[0].offset,
      typeLayouts: &l
    )
    c.compose(
      i8, at: l[t3].parts[0].offset + l[t2].parts[0].offset + l[t1].parts[1].offset,
      typeLayouts: &l
    )
    c.compose(i8, at: l[t3].parts[0].offset + l[t2].parts[1].offset, typeLayouts: &l)
    c.compose(i8, at: l[t3].parts[1].offset, typeLayouts: &l)

    c.composeUpwards(along: [
        .init(offset: l[t3].parts[0].offset, type: t2),
        .init(offset: l[t3].parts[0].offset + l[t2].parts[0].offset, type: t1),
        .init(
          offset: l[t3].parts[0].offset + l[t2].parts[0].offset + l[t1].parts[1].offset, type: i8),
      ],
      in: m[p.allocation], typeLayouts: &l
    )

    XCTAssertEqual(
      c.region(enclosing: l[t3].parts[0].offset, typeLayouts: &l),
      .init(offset: l[t3].parts[0].offset, type: t2))
    XCTAssertEqual(
      c.region(enclosing: l[t3].parts[1].offset, typeLayouts: &l),
      .init(offset: l[t3].parts[1].offset, type: i8))
  }
}
