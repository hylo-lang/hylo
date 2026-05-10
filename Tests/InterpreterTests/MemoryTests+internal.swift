import XCTest
import TestUtils
import FrontEnd
@testable import Interpreter

final class InterpreterMemoryInternalTests: XCTestCase {

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
    let w = try m.access(a, with: .set)
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

    let ws = try m.access(s, with: .set)
    try m.store(v, in: ws)
    try m.end(ws)

    let rs = try m.access(s, with: .let)
    let wd = try m.access(d, with: .set)
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

    // TODO: should throw when try to copy from non-composed region.
    // var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    // let s = m.allocate(^BuiltinType.i(8))
    // let d = m.allocate(^BuiltinType.i(8))
    // check(throws: Memory.Error.noComposedPart(at: s.address)) {
    //   try m.copy(s, to: d)
    // }
  }

  func check(storeAndLoadSucceeds v: BuiltinValue, asType t: BuiltinType) throws {
    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(^t)
    let w = try m.access(p, with: .set)
    try m.store(v, in: w)
    try m.end(w)
    let r = try m.access(p, with: .let)
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

    // TODO: test for loading as different type than stored.
    // try check(loading: .i1(true), asType: .i(8), throws: .noComposedPart(...))
  }

}
