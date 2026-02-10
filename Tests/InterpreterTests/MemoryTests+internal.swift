import XCTest
import TestUtils
import FrontEnd
@testable import Interpreter

final class InterpreterMemoryInternalTests: XCTestCase {

  func test_requireInitialized() throws {
    var layouts = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())
    let void_ = AnyType.void
    let voidPair = ^TupleType(types: [.void, .void])

    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(voidPair).address

    let voidPairPart0 = layouts[voidPair].partParentages.first!

    TestUtils.check(throws: Memory.Error.noComposedPart(at: p, voidPairPart0)) {
      try m.allocation[p.allocation]!
        .requireComposed(part: voidPairPart0, baseOffset: 0, region: 0)
    }

    try m.compose(void_, at: p + layouts[voidPair].parts[0].offset)
    // It should be possible to initialize both parts at the same address
    try m.compose(void_, at: p + layouts[voidPair].parts[1].offset)
    try m.compose(voidPair, at: p)

    let i8Pair = layouts[^TupleType(types: [.builtin(.i(8)), .builtin(.i(8))])]
    let i8PairPart0 = i8Pair.partParentages.first!

    TestUtils.check(throws: Memory.Error.partType(voidPair, part: i8PairPart0)) {
      try m.allocation[p.allocation]!
        .requireComposed(part: i8PairPart0, baseOffset: 0, region: 0)
    }
  }

  func testFormingPointerToLastByteOfAllocation() throws {
    var memory = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let a = memory.allocate(^BuiltinType.i(8))

    memory[a.allocation].withUnsafeMutablePointer(to: UInt8.self, at: 0) { p in
      p.pointee = 2
    }
    memory[a.allocation].withUnsafePointer(to: UInt8.self, at: 0) { p in
      XCTAssertEqual(p.pointee, 2)
    }

  }

  private func check<T: Equatable>(storing v: BuiltinValue, asType t: BuiltinType, yields e: T)
    throws
  {
    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let a = m.allocate(^t)
    try m.store(v, in: a)
    m[a.allocation].withUnsafePointer(to: T.self, at: 0) {
      XCTAssertEqual($0.pointee, e)
    }
  }

  func testStoringBuiltinValueInAllocation() throws {
    try check(storing: .i1(true), asType: .i(1), yields: true)
    try check(storing: .i8(8), asType: .i(8), yields: UInt8(8))
    try check(storing: .i16(8), asType: .i(16), yields: UInt16(8))
    try check(storing: .i32(8), asType: .i(32), yields: UInt32(8))
    try check(storing: .i64(8), asType: .i(64), yields: UInt64(8))
    try check(storing: .i128(8), asType: .i(128), yields: UInt128(8))
  }

  func check<T: Equatable>(copying v: BuiltinValue, asType t: BuiltinType, yields e: T) throws {
    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let s = m.allocate(^t)
    let d = m.allocate(^t)

    try m.store(v, in: s)
    try m.compose(^t, at: s.address)  // FIXME: This should not be needed after store.
    try m.copy(s, to: d)

    m[d.allocation].withUnsafePointer(to: T.self, at: 0) {
      XCTAssertEqual($0.pointee, e)
    }
  }

  func testMemoryCopy() throws {
    try check(copying: .i1(true), asType: .i(1), yields: true)
    try check(copying: .i8(8), asType: .i(8), yields: UInt8(8))
    try check(copying: .i16(8), asType: .i(16), yields: UInt16(8))
    try check(copying: .i32(8), asType: .i(32), yields: UInt32(8))
    try check(copying: .i64(8), asType: .i(64), yields: UInt64(8))
    try check(copying: .i128(8), asType: .i(128), yields: UInt128(8))

    // TODO: should throw when try to copy from non-composed region.
    // var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    // let s = m.allocate(^BuiltinType.i(8))
    // let d = m.allocate(^BuiltinType.i(8))
    // check(throws: Memory.Error.noComposedPart(at: s.address)) {
    //   try m.copy(s, to: d)
    // }
  }
}
