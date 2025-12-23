import XCTest
import TestUtils
import FrontEnd
@testable import Interpreter

final class InterpreterMemoryInternalTests: XCTestCase {

  func testTypedAllocation() {
    var l = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())
    var m = Memory()
    let a = m.allocate(l[^BuiltinType.i(32)])
    let e = Memory.Allocation.ComposedRegion(offset: 0, type: ^BuiltinType.i(32))
    XCTAssertEqual(m[a.allocation].composedRegion(containingOffset: 0), e)
    XCTAssertEqual(m[a.allocation].composedRegion(containingOffset: 1), e)
  }

  func test_requireInitialized() throws {
    var layouts = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())
    let void_ = layouts[AnyType.void]
    let voidPair = layouts[^TupleType(types: [.void, .void])]

    var m = Memory()
    let p = m.allocate(voidPair.size, bytesWithAlignment: voidPair.alignment)

    let voidPairPart0 = voidPair.partParentages.first!

    check(throws: Memory.Error.noComposedPart(at: p, voidPairPart0)) {
      try m.allocation[p.allocation]!
        .requireComposed(part: voidPairPart0, baseOffset: 0, region: 0)
    }

    try m.compose(void_, at: p + voidPair.parts[0].offset)
    // It should be possible to initialize both parts at the same address
    try m.compose(void_, at: p + voidPair.parts[1].offset)
    try m.compose(voidPair, at: p)

    let i8Pair = layouts[^TupleType(types: [.builtin(.i(8)), .builtin(.i(8))])]
    let i8PairPart0 = i8Pair.partParentages.first!

    check(throws: Memory.Error.partType(voidPair.type, part: i8PairPart0)) {
      try m.allocation[p.allocation]!
        .requireComposed(part: i8PairPart0, baseOffset: 0, region: 0)
    }
  }

  func testFormingPointerToLastByteOfAllocation() throws {
    var memory = Memory()
    let a = memory.allocate(1, bytesWithAlignment: 1)

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
    var m = Memory()
    var l = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())
    let a = Address(startLocation: m.allocate(l[^t]), type: l[^t])
    try m.store(v, at: a, with: &l)
    m[a.startLocation.allocation].withUnsafePointer(to: T.self, at: 0) {
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

    var m = Memory()
    var l = TypeLayoutCache(typesIn: TypedProgram.empty, for: UnrealABI())
    let a = Address(startLocation: m.allocate(l[^BuiltinType.i(8)]), type: l[^BuiltinType.i(8)])
    check(throws: Memory.Error.invalidTypeAccess(l[^BuiltinType.i(1)], at: a.startLocation)) {
      try m.store(BuiltinValue.i1(true), at: a, with: &l)
    }
  }
}
