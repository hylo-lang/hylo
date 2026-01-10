import XCTest
import TestUtils
import FrontEnd
@testable import Interpreter

final class InterpreterMemoryInternalTests: XCTestCase {

  func test_requireInitialized() throws {
    let void_ = AnyType.void
    let voidPair = ^TupleType(types: [.void, .void])

    var m = Memory(TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI()))
    let p = m.allocate(voidPair)
    let l = m.typeLayouts[voidPair]

    let voidPairPart0 = l.partParentages.first!

    check(throws: Memory.Error.noComposedPart(at: p, voidPairPart0)) {
      try m.allocation[p.allocation]!
        .requireComposed(part: voidPairPart0, baseOffset: 0, region: 0)
    }

    try m.compose(p.after(l.parts[0].offset, bytesHavingType: void_))
    // It should be possible to initialize both parts at the same address
    try m.compose(p.after(l.parts[1].offset, bytesHavingType: void_))
    try m.compose(p)

    let i8Pair = ^TupleType(types: [.builtin(.i(8)), .builtin(.i(8))])
    let i8PairPart0 = m.typeLayouts[i8Pair].partParentages.first!

    check(throws: Memory.Error.partType(voidPair, part: i8PairPart0)) {
      try m.allocation[p.allocation]!
        .requireComposed(part: i8PairPart0, baseOffset: 0, region: 0)
    }
  }

  func testFormingPointerToLastByteOfAllocation() throws {
    var m = Memory(TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI()))
    let a = m.allocate(^BuiltinType.i(8))

    m[a.allocation].withUnsafeMutablePointer(to: UInt8.self, at: 0) { p in
      p.pointee = 2
    }
    m[a.allocation].withUnsafePointer(to: UInt8.self, at: 0) { p in
      XCTAssertEqual(p.pointee, 2)
    }

  }

  private func assertStoring<T: Equatable>(_ v: BuiltinValue, asType t: BuiltinType, yields e: T)
    throws
  {
    var m = Memory(TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI()))
    let a = m.allocate(^t)
    try m.store(v, at: a)
    m[a.allocation].withUnsafePointer(to: T.self, at: 0) {
      XCTAssertEqual($0.pointee, e)
    }
  }

  func testStoringBuiltinValueInAllocation() throws {
    try assertStoring(BuiltinValue.i1(true), asType: BuiltinType.i(1), yields: true)
    try assertStoring(BuiltinValue.i8(8), asType: BuiltinType.i(8), yields: UInt8(8))
    try assertStoring(BuiltinValue.i16(8), asType: BuiltinType.i(16), yields: UInt16(8))
    try assertStoring(BuiltinValue.i32(8), asType: BuiltinType.i(32), yields: UInt32(8))
    try assertStoring(BuiltinValue.i64(8), asType: BuiltinType.i(64), yields: UInt64(8))
    try assertStoring(BuiltinValue.i128(8), asType: BuiltinType.i(128), yields: UInt128(8))
  }
}
