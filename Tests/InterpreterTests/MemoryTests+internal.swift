import XCTest
import TestUtils
import FrontEnd
@testable import Interpreter

final class InterpreterMemoryInternalTests: XCTestCase {

  func test_requireInitialized() throws {
    var layouts = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())
    let void_ = AnyType.void
    let voidPair = ^TupleType(types: [.void, .void])

    var m = Memory(layouts)
    let p = m.allocate(voidPair).address

    let voidPairPart0 = layouts[voidPair].partParentages.first!

    check(throws: Memory.Error.noComposedPart(at: p, voidPairPart0)) {
      try m.allocation[p.allocation]!
        .requireComposed(part: voidPairPart0, baseOffset: 0, region: 0)
    }

    try m.compose(void_, at: p + layouts[voidPair].parts[0].offset)
    // It should be possible to initialize both parts at the same address
    try m.compose(void_, at: p + layouts[voidPair].parts[1].offset)
    try m.compose(voidPair, at: p)

    let i8Pair = layouts[^TupleType(types: [.builtin(.i(8)), .builtin(.i(8))])]
    let i8PairPart0 = i8Pair.partParentages.first!

    check(throws: Memory.Error.partType(voidPair, part: i8PairPart0)) {
      try m.allocation[p.allocation]!
        .requireComposed(part: i8PairPart0, baseOffset: 0, region: 0)
    }
  }

  func testFormingPointerToLastByteOfAllocation() throws {
    var memory = Memory(TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI()))
    let a = memory.allocate(^BuiltinType.i(8))

    memory[a.allocation].withUnsafeMutablePointer(to: UInt8.self, at: 0) { p in
      p.pointee = 2
    }
    memory[a.allocation].withUnsafePointer(to: UInt8.self, at: 0) { p in
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
