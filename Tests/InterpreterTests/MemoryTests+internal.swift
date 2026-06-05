import XCTest
import TestUtils
import FrontEnd
@testable import Interpreter

final class InterpreterMemoryInternalTests: XCTestCase {

  let i8 = ^BuiltinType.i(8)
  let i16 = ^BuiltinType.i(16)

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
    let w = try m.begin(.set, to: a)
    try m.store(v, in: w)
    try m.end(w)
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

    let ws = try m.begin(.set, to: s)
    try m.store(v, in: ws)
    try m.end(ws)

    let rs = try m.begin(.let, to: s)
    let wd = try m.begin(.set, to: d)
    try m.copy(rs, to: wd)
    try m.end(wd)
    try m.end(rs)

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
  }

  func testCopyingFromUninitializedObject() throws {
    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let source = m.allocate(i8)
    try m.bind(source.address, to: i8)
    let t = try m.begin(.set, to: source)
    try m.store(.i8(2), in: t)
    try m.end(t)
    let destination = m.allocate(i8)
    try m.bind(destination.address, to: i8)
    let s = try m.begin(.sink, to: source)
    try m.markDeinitialized(source)
    let d = try m.begin(.set, to: destination)
    TestUtils.check(throws: MemorySafetyValidator.Error.readFromIncomplete(source)) {
      try m.copy(s, to: d)
    }
  }

  func check(storeAndLoadSucceeds v: BuiltinValue, asType t: BuiltinType) throws {
    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(^t)
    let w = try m.begin(.set, to: p)
    try m.store(v, in: w)
    try m.end(w)
    let r = try m.begin(.let, to: p)
    XCTAssertEqual(try m.builtinValue(in: r), v)
    try m.end(r)
  }

  func testLoad() throws {
    try check(storeAndLoadSucceeds: .i1(true), asType: .i(1))
    try check(storeAndLoadSucceeds: .i8(8), asType: .i(8))
    try check(storeAndLoadSucceeds: .i16(8), asType: .i(16))
    try check(storeAndLoadSucceeds: .i32(8), asType: .i(32))
    try check(storeAndLoadSucceeds: .i64(8), asType: .i(64))
    try check(storeAndLoadSucceeds: .i128(8), asType: .i(128))
  }

  func testFormingAccessOfInvalidType() throws {
    for e in [.let, .set, .inout, .sink] as [AccessEffect] {
      var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
      let p = m.allocate(i16)
      let i8Place = Memory.Place(allocation: p.allocation, offset: p.offset, type: i8)
      try m.bind(p.address, to: p.type)
      TestUtils.check(throws: MemorySafetyValidator.Error.notContained(i8Place, in: p)) {
        _ = try m.begin(e, to: i8Place)
      }
    }

    for e in [.let, .set, .inout, .sink] as [AccessEffect] {
      var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
      let p = m.allocate(^TupleType(types: [i8, i8]))
      let i8Place = Memory.Place(
        allocation: p.allocation, offset: p.offset, type: ^TupleType(types: [i8]))
      try m.bind(p.address, to: p.type)
      TestUtils.check(throws: MemorySafetyValidator.Error.notContained(i8Place, in: p)) {
        _ = try m.begin(e, to: i8Place)
      }
    }
  }

}
