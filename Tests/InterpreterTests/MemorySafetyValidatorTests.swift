import FrontEnd
import TestUtils
import XCTest

@testable import Interpreter

final class MemorySafetyValidatorTests: XCTestCase {
  let i8 = ^BuiltinType.i(8)
  let i16 = ^BuiltinType.i(16)
  let i32 = ^BuiltinType.i(32)
  let i64 = ^BuiltinType.i(64)
  let nonSetAccesses: [AccessEffect] = [.let, .inout, .sink]

  typealias Error = MemorySafetyValidator.Error

  func testBindingTypeToInvalidMemoryRegion() throws {
    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(i32)
    let a = p.allocation
    var v = MemorySafetyValidator(
      memory: withUnsafeMutablePointer(to: &m) { $0 }, allocation: a)

    check(throws: Memory.Error.alignment(p.address + 1, for: m.typeLayouts[i16])) {
      _ = try v.beginAccess(.set, at: .init(allocation: a, offset: 1, type: i16))
    }

    check(throws: Memory.Error.bounds(p.address, for: m.typeLayouts[i64], allocationSize: 4)) {
      _ = try v.beginAccess(.set, at: .init(allocation: a, offset: 0, type: i64))
    }

    _ = try v.beginAccess(.set, at: .init(allocation: a, offset: 2, type: i16))
    check(throws: ReservedTypeRegions.Error.regionAlreadyBound(to: i16)) {
      _ = try v.beginAccess(.set, at: .init(allocation: a, offset: 0, type: i32))
    }
  }

  func testCreatingNonSetAccessToInCompleteRegion() throws {
    for k in nonSetAccesses {
      var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
      let p = m.allocate(i32)
      let a = p.allocation
      var v = MemorySafetyValidator(
        memory: withUnsafeMutablePointer(to: &m) { $0 }, allocation: a)

      let t = ^TupleType(types: [i8, i8])

      let whole = Memory.Place(allocation: a, offset: 0, type: t)
      let firstPart = Memory.Place(allocation: a, offset: 0, type: i8)
      let secondPart = Memory.Place(allocation: a, offset: 1, type: i8)

      // No parts are initialized
      _ = try v.beginAccess(.set, at: whole)
      check(throws: Error.accessToIncomplete(whole, kind: k)) {
        _ = try v.beginAccess(k, at: whole)
      }
      check(throws: Error.accessToIncomplete(firstPart, kind: k)) {
        _ = try v.beginAccess(k, at: firstPart)
      }
      check(throws: Error.accessToIncomplete(secondPart, kind: k)) {
        _ = try v.beginAccess(k, at: secondPart)
      }

      // First part is initialized
      try v.markInitialized(firstPart)
      check(throws: Error.accessToIncomplete(whole, kind: k)) {
        _ = try v.beginAccess(k, at: whole)
      }
      let f = try v.beginAccess(k, at: firstPart)
      try v.endAccess(f, at: firstPart)
      check(throws: Error.accessToIncomplete(secondPart, kind: k)) {
        _ = try v.beginAccess(k, at: secondPart)
      }

      // Both parts are initialized
      if k == .sink { try v.markInitialized(firstPart) }
      try v.markInitialized(secondPart)
      _ = try v.beginAccess(k, at: whole)
      _ = try v.beginAccess(k, at: firstPart)
      _ = try v.beginAccess(k, at: secondPart)
    }
  }

  func testCreatingSetAccessToPartiallyInitializedRegion() throws {
    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(i32)
    let a = p.allocation
    var v = MemorySafetyValidator(
      memory: withUnsafeMutablePointer(to: &m) { $0 }, allocation: a)

    let t = ^TupleType(types: [i8, i8])

    let whole = Memory.Place(allocation: a, offset: 0, type: t)
    let firstPart = Memory.Place(allocation: a, offset: 0, type: i8)
    let secondPart = Memory.Place(allocation: a, offset: 1, type: i8)

    _ = try v.beginAccess(.set, at: whole)

    try v.markInitialized(firstPart)
    check(throws: Error.setAccessToPartiallyComplete(firstPart)) {
      _ = try v.beginAccess(.set, at: firstPart)
    }
    check(throws: Error.setAccessToPartiallyComplete(whole)) {
      _ = try v.beginAccess(.set, at: whole)
    }

    try v.markInitialized(secondPart)
    check(throws: Error.setAccessToPartiallyComplete(firstPart)) {
      _ = try v.beginAccess(.set, at: firstPart)
    }
    check(throws: Error.setAccessToPartiallyComplete(secondPart)) {
      _ = try v.beginAccess(.set, at: secondPart)
    }
    check(throws: Error.setAccessToPartiallyComplete(whole)) {
      _ = try v.beginAccess(.set, at: whole)
    }
  }

  func testEndingAccessOnCompleteObjects() throws {
    for k in nonSetAccesses {
      var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
      let p = m.allocate(i32)
      let a = p.allocation
      var v = MemorySafetyValidator(
        memory: withUnsafeMutablePointer(to: &m) { $0 }, allocation: a)

      let t = ^TupleType(types: [i8, i8])

      let whole = Memory.Place(allocation: a, offset: 0, type: t)
      let firstPart = Memory.Place(allocation: a, offset: 0, type: i8)
      let secondPart = Memory.Place(allocation: a, offset: 1, type: i8)

      let x = try v.beginAccess(.set, at: whole)
      try v.markInitialized(firstPart)
      try v.markInitialized(secondPart)
      try v.endAccess(x, at: whole)

      let w = try v.beginAccess(k, at: whole)
      try v.endAccess(w, at: whole)
      if k == .sink {
        try v.markInitialized(firstPart)
        try v.markInitialized(secondPart)
      }
      let f = try v.beginAccess(k, at: firstPart)
      try v.endAccess(f, at: firstPart)
      let s = try v.beginAccess(k, at: secondPart)
      try v.endAccess(s, at: secondPart)
    }
  }

  func testEndingAccessOnIncompleteObjects() throws {
    for k in [.let, .inout] as [AccessEffect] {
      var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
      let p = m.allocate(i32)
      let a = p.allocation
      var v = MemorySafetyValidator(
        memory: withUnsafeMutablePointer(to: &m) { $0 }, allocation: a)

      let t = ^TupleType(types: [i8, i8])

      let whole = Memory.Place(allocation: a, offset: 0, type: t)
      let firstPart = Memory.Place(allocation: a, offset: 0, type: i8)
      let secondPart = Memory.Place(allocation: a, offset: 1, type: i8)

      let x = try v.beginAccess(.set, at: whole)
      check(throws: Error.endAccessToIncomplete(whole, kind: .set)) {
        try v.endAccess(x, at: whole)
      }
      try v.markInitialized(firstPart)
      check(throws: Error.endAccessToIncomplete(whole, kind: .set)) {
        try v.endAccess(x, at: whole)
      }
      try v.markInitialized(secondPart)
      try v.endAccess(x, at: whole)

      let w = try v.beginAccess(k, at: whole)
      let s = try v.beginAccess(.sink, at: firstPart)
      try v.endAccess(s, at: firstPart)
      check(throws: Error.endAccessToIncomplete(whole, kind: k)) {
        try v.endAccess(w, at: whole)
      }
    }
  }

  func testMarkInitializedOnAlreadyInitializedObject() throws {
    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(i32)
    let a = p.allocation
    var v = MemorySafetyValidator(
      memory: withUnsafeMutablePointer(to: &m) { $0 }, allocation: a)

    let t = ^TupleType(types: [i8, i8])

    let whole = Memory.Place(allocation: a, offset: 0, type: t)
    let firstPart = Memory.Place(allocation: a, offset: 0, type: i8)
    let secondPart = Memory.Place(allocation: a, offset: 1, type: i8)

    let x = try v.beginAccess(.set, at: whole)
    try v.markInitialized(firstPart)
    try v.markInitialized(secondPart)
    try v.markInitialized(firstPart)
    try v.endAccess(x, at: whole)

    _ = try v.beginAccess(.let, at: whole)
  }

  func testRequireCanReadFromActiveAccesses() throws {
    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(i32)
    let a = p.allocation
    var v = MemorySafetyValidator(
      memory: withUnsafeMutablePointer(to: &m) { $0 }, allocation: a)

    let t = ^TupleType(types: [i8, i8])

    let whole = Memory.Place(allocation: a, offset: 0, type: t)
    let firstPart = Memory.Place(allocation: a, offset: 0, type: i8)
    let secondPart = Memory.Place(allocation: a, offset: 1, type: i8)

    let x = try v.beginAccess(.set, at: whole)
    try v.markInitialized(firstPart)
    try v.markInitialized(secondPart)
    try v.endAccess(x, at: whole)

    let w = try v.beginAccess(.sink, at: whole)
    try v.requireCanRead(from: whole, using: w)
    try v.requireCanRead(from: firstPart, using: w)
    try v.requireCanRead(from: secondPart, using: w)

    let f = try v.beginAccess(.sink, at: firstPart)
    try v.endAccess(f, at: firstPart)
    check(throws: Error.readFromIncomplete(whole)) {
      try v.requireCanRead(from: whole, using: w)
    }
    check(throws: Error.readFromIncomplete(firstPart)) {
      try v.requireCanRead(from: firstPart, using: w)
    }
    try v.requireCanRead(from: secondPart, using: w)

    let s = try v.beginAccess(.sink, at: secondPart)
    try v.endAccess(s, at: secondPart)
    check(throws: Error.readFromIncomplete(whole)) {
      try v.requireCanRead(from: whole, using: w)
    }
    check(throws: Error.readFromIncomplete(firstPart)) {
      try v.requireCanRead(from: firstPart, using: w)
    }
    check(throws: Error.readFromIncomplete(secondPart)) {
      try v.requireCanRead(from: secondPart, using: w)
    }
  }

  func testRequireCanReadFromInactiveAccesses() throws {
    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(i32)
    let a = p.allocation
    var v = MemorySafetyValidator(
      memory: withUnsafeMutablePointer(to: &m) { $0 }, allocation: a)

    let t = ^TupleType(types: [i8, i8])

    let whole = Memory.Place(allocation: a, offset: 0, type: t)
    let firstPart = Memory.Place(allocation: a, offset: 0, type: i8)
    let secondPart = Memory.Place(allocation: a, offset: 1, type: i8)

    let x = try v.beginAccess(.set, at: whole)
    try v.markInitialized(firstPart)
    try v.markInitialized(secondPart)
    try v.endAccess(x, at: whole)

    let w = try v.beginAccess(.sink, at: whole)
    let w1 = try v.beginAccess(.sink, at: whole)
    check(throws: AccessTracker.Error.overlappingExclusiveAccessExists(for: whole.typedRegion)) {
      try v.requireCanRead(from: whole, using: w)
    }
    check(throws: AccessTracker.Error.overlappingExclusiveAccessExists(for: whole.typedRegion)) {
      try v.requireCanRead(from: firstPart, using: w)
    }
    try v.requireCanRead(from: whole, using: w1)
    try v.requireCanRead(from: firstPart, using: w1)
    let f = try v.beginAccess(.sink, at: firstPart)
    check(throws: AccessTracker.Error.overlappingExclusiveAccessExists(for: whole.typedRegion)) {
      try v.requireCanRead(from: firstPart, using: w)
    }
    check(throws: AccessTracker.Error.overlappingExclusiveAccessExists(for: whole.typedRegion)) {
      try v.requireCanRead(from: firstPart, using: w1)
    }
    try v.requireCanRead(from: firstPart, using: f)
    let f1 = try v.beginAccess(.sink, at: firstPart)
    check(throws: AccessTracker.Error.overlappingExclusiveAccessExists(for: whole.typedRegion)) {
      try v.requireCanRead(from: firstPart, using: w)
    }
    check(throws: AccessTracker.Error.overlappingExclusiveAccessExists(for: whole.typedRegion)) {
      try v.requireCanRead(from: firstPart, using: w1)
    }
    check(throws: AccessTracker.Error.overlappingExclusiveAccessExists(for: firstPart.typedRegion)) {
      try v.requireCanRead(from: firstPart, using: f)
    }
    try v.requireCanRead(from: firstPart, using: f1)
  }

  func testRequireWriteToUsingActiveAccess() throws {
    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(i32)
    let a = p.allocation
    var v = MemorySafetyValidator(
      memory: withUnsafeMutablePointer(to: &m) { $0 }, allocation: a)

    let t = ^TupleType(types: [i8, i8])

    let whole = Memory.Place(allocation: a, offset: 0, type: t)
    let firstPart = Memory.Place(allocation: a, offset: 0, type: i8)
    let secondPart = Memory.Place(allocation: a, offset: 1, type: i8)

    let x = try v.beginAccess(.set, at: whole)
    try v.requireCanWrite(to: whole, using: x)
    try v.requireCanWrite(to: firstPart, using: x)
    try v.requireCanWrite(to: secondPart, using: x)
    try v.markInitialized(firstPart)
    try v.requireCanWrite(to: whole, using: x)
    try v.requireCanWrite(to: firstPart, using: x)
    try v.requireCanWrite(to: secondPart, using: x)
    try v.markInitialized(secondPart)
    try v.requireCanWrite(to: whole, using: x)
    try v.requireCanWrite(to: firstPart, using: x)
    try v.requireCanWrite(to: secondPart, using: x)
    try v.endAccess(x, at: whole)
  }

  func testRequireWriteToUsingInactiveAccess() throws {
    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(i32)
    let a = p.allocation
    var v = MemorySafetyValidator(
      memory: withUnsafeMutablePointer(to: &m) { $0 }, allocation: a)

    let t = ^TupleType(types: [i8, i8])

    let whole = Memory.Place(allocation: a, offset: 0, type: t)
    let firstPart = Memory.Place(allocation: a, offset: 0, type: i8)
    let secondPart = Memory.Place(allocation: a, offset: 1, type: i8)

    let x = try v.beginAccess(.set, at: whole)
    try v.markInitialized(firstPart)
    try v.markInitialized(secondPart)
    try v.endAccess(x, at: whole)

    let w = try v.beginAccess(.sink, at: whole)
    let w1 = try v.beginAccess(.sink, at: whole)
    check(throws: AccessTracker.Error.overlappingExclusiveAccessExists(for: whole.typedRegion)) {
      try v.requireCanWrite(to: whole, using: w)
    }
    check(throws: AccessTracker.Error.overlappingExclusiveAccessExists(for: whole.typedRegion)) {
      try v.requireCanWrite(to: firstPart, using: w)
    }
    try v.requireCanWrite(to: whole, using: w1)
    try v.requireCanWrite(to: firstPart, using: w1)
    let f = try v.beginAccess(.sink, at: firstPart)
    check(throws: AccessTracker.Error.overlappingExclusiveAccessExists(for: whole.typedRegion)) {
      try v.requireCanWrite(to: firstPart, using: w)
    }
    check(throws: AccessTracker.Error.overlappingExclusiveAccessExists(for: whole.typedRegion)) {
      try v.requireCanWrite(to: firstPart, using: w1)
    }
    try v.requireCanWrite(to: firstPart, using: f)
    let f1 = try v.beginAccess(.sink, at: firstPart)
    check(throws: AccessTracker.Error.overlappingExclusiveAccessExists(for: whole.typedRegion)) {
      try v.requireCanWrite(to: firstPart, using: w)
    }
    check(throws: AccessTracker.Error.overlappingExclusiveAccessExists(for: whole.typedRegion)) {
      try v.requireCanWrite(to: firstPart, using: w1)
    }
    check(throws: AccessTracker.Error.overlappingExclusiveAccessExists(for: firstPart.typedRegion)) {
      try v.requireCanWrite(to: firstPart, using: f)
    }
    try v.requireCanWrite(to: firstPart, using: f1)
  }

}

private extension Memory.Place {
  var typedRegion: Memory.Allocation.TypedRegion {
    .init(startOffset: offset, type: type)
  }
}
