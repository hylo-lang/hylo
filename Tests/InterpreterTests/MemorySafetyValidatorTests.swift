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
  typealias AccessTrackerError = AccessTracker<Memory.Allocation.TypedRegion>.Error

  func testBindingTypeToInvalidMemoryRegion() throws {
    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(i32)
    let a = p.allocation
    var v = MemorySafetyValidator(a)

    check(throws: Memory.Error.alignment(p.address + 1, for: m.typeLayouts[i16])) {
      _ = try v.beginAccess(
        .set, at: .init(allocation: a, offset: 1, type: i16), in: m[a], typeLayouts: &m.typeLayouts)
    }

    check(throws: Memory.Error.bounds(p.address, for: m.typeLayouts[i64], allocationSize: 4)) {
      _ = try v.beginAccess(
        .set, at: .init(allocation: a, offset: 0, type: i64), in: m[a], typeLayouts: &m.typeLayouts)
    }

    _ = try v.beginAccess(
      .set, at: .init(allocation: a, offset: 2, type: i16), in: m[a], typeLayouts: &m.typeLayouts)
    check(throws: ReservedTypeRegions.Error.regionAlreadyBound(to: i16)) {
      _ = try v.beginAccess(
        .set, at: .init(allocation: a, offset: 0, type: i32), in: m[a], typeLayouts: &m.typeLayouts)
    }
  }

  func testCreatingNonSetAccessToInCompleteRegion() throws {
    for k in nonSetAccesses {
      var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
      let p = m.allocate(i32)
      let a = p.allocation
      var v = MemorySafetyValidator(a)

      let t = ^TupleType(types: [i8, i8])

      let whole = Memory.Place(allocation: a, offset: 0, type: t)
      let firstPart = Memory.Place(allocation: a, offset: 0, type: i8)
      let secondPart = Memory.Place(allocation: a, offset: 1, type: i8)

      // No parts are initialized
      _ = try v.beginAccess(.set, at: whole, in: m[a], typeLayouts: &m.typeLayouts)
      check(throws: Error.accessToIncomplete(whole, capability: k)) {
        _ = try v.beginAccess(k, at: whole, in: m[a], typeLayouts: &m.typeLayouts)
      }
      check(throws: Error.accessToIncomplete(firstPart, capability: k)) {
        _ = try v.beginAccess(k, at: firstPart, in: m[a], typeLayouts: &m.typeLayouts)
      }
      check(throws: Error.accessToIncomplete(secondPart, capability: k)) {
        _ = try v.beginAccess(k, at: secondPart, in: m[a], typeLayouts: &m.typeLayouts)
      }

      // First part is initialized
      try v.markInitialized(firstPart, in: m[a], typeLayouts: &m.typeLayouts)
      check(throws: Error.accessToIncomplete(whole, capability: k)) {
        _ = try v.beginAccess(k, at: whole, in: m[a], typeLayouts: &m.typeLayouts)
      }
      let f = try v.beginAccess(k, at: firstPart, in: m[a], typeLayouts: &m.typeLayouts)
      try v.endAccess(f, at: firstPart, in: m[a], typeLayouts: &m.typeLayouts)
      check(throws: Error.accessToIncomplete(secondPart, capability: k)) {
        _ = try v.beginAccess(k, at: secondPart, in: m[a], typeLayouts: &m.typeLayouts)
      }

      // Both parts are initialized
      if k == .sink { try v.markInitialized(firstPart, in: m[a], typeLayouts: &m.typeLayouts) }
      try v.markInitialized(secondPart, in: m[a], typeLayouts: &m.typeLayouts)
      _ = try v.beginAccess(k, at: whole, in: m[a], typeLayouts: &m.typeLayouts)
      _ = try v.beginAccess(k, at: firstPart, in: m[a], typeLayouts: &m.typeLayouts)
      _ = try v.beginAccess(k, at: secondPart, in: m[a], typeLayouts: &m.typeLayouts)
    }
  }

  func testCreatingSetAccessToPartiallyInitializedRegion() throws {
    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(i32)
    let a = p.allocation
    var v = MemorySafetyValidator(a)

    let t = ^TupleType(types: [i8, i8])

    let whole = Memory.Place(allocation: a, offset: 0, type: t)
    let firstPart = Memory.Place(allocation: a, offset: 0, type: i8)
    let secondPart = Memory.Place(allocation: a, offset: 1, type: i8)

    _ = try v.beginAccess(.set, at: whole, in: m[a], typeLayouts: &m.typeLayouts)

    try v.markInitialized(firstPart, in: m[a], typeLayouts: &m.typeLayouts)
    check(throws: Error.setAccessToPartiallyComplete(firstPart)) {
      _ = try v.beginAccess(.set, at: firstPart, in: m[a], typeLayouts: &m.typeLayouts)
    }
    check(throws: Error.setAccessToPartiallyComplete(whole)) {
      _ = try v.beginAccess(.set, at: whole, in: m[a], typeLayouts: &m.typeLayouts)
    }

    try v.markInitialized(secondPart, in: m[a], typeLayouts: &m.typeLayouts)
    check(throws: Error.setAccessToPartiallyComplete(firstPart)) {
      _ = try v.beginAccess(.set, at: firstPart, in: m[a], typeLayouts: &m.typeLayouts)
    }
    check(throws: Error.setAccessToPartiallyComplete(secondPart)) {
      _ = try v.beginAccess(.set, at: secondPart, in: m[a], typeLayouts: &m.typeLayouts)
    }
    check(throws: Error.setAccessToPartiallyComplete(whole)) {
      _ = try v.beginAccess(.set, at: whole, in: m[a], typeLayouts: &m.typeLayouts)
    }
  }

  func testEndingAccessOnCompleteObjects() throws {
    for k in nonSetAccesses {
      var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
      let p = m.allocate(i32)
      let a = p.allocation
      var v = MemorySafetyValidator(a)

      let t = ^TupleType(types: [i8, i8])

      let whole = Memory.Place(allocation: a, offset: 0, type: t)
      let firstPart = Memory.Place(allocation: a, offset: 0, type: i8)
      let secondPart = Memory.Place(allocation: a, offset: 1, type: i8)

      let x = try v.beginAccess(.set, at: whole, in: m[a], typeLayouts: &m.typeLayouts)
      try v.markInitialized(firstPart, in: m[a], typeLayouts: &m.typeLayouts)
      try v.markInitialized(secondPart, in: m[a], typeLayouts: &m.typeLayouts)
      try v.endAccess(x, at: whole, in: m[a], typeLayouts: &m.typeLayouts)

      let w = try v.beginAccess(k, at: whole, in: m[a], typeLayouts: &m.typeLayouts)
      try v.endAccess(w, at: whole, in: m[a], typeLayouts: &m.typeLayouts)
      if k == .sink {
        try v.markInitialized(firstPart, in: m[a], typeLayouts: &m.typeLayouts)
        try v.markInitialized(secondPart, in: m[a], typeLayouts: &m.typeLayouts)
      }
      let f = try v.beginAccess(k, at: firstPart, in: m[a], typeLayouts: &m.typeLayouts)
      try v.endAccess(f, at: firstPart, in: m[a], typeLayouts: &m.typeLayouts)
      let s = try v.beginAccess(k, at: secondPart, in: m[a], typeLayouts: &m.typeLayouts)
      try v.endAccess(s, at: secondPart, in: m[a], typeLayouts: &m.typeLayouts)
    }
  }

  func testEndingAccessOnIncompleteObjects() throws {
    for k in [.let, .inout] as [AccessEffect] {
      var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
      let p = m.allocate(i32)
      let a = p.allocation
      var v = MemorySafetyValidator(a)

      let t = ^TupleType(types: [i8, i8])

      let whole = Memory.Place(allocation: a, offset: 0, type: t)
      let firstPart = Memory.Place(allocation: a, offset: 0, type: i8)
      let secondPart = Memory.Place(allocation: a, offset: 1, type: i8)

      let x = try v.beginAccess(.set, at: whole, in: m[a], typeLayouts: &m.typeLayouts)
      check(throws: Error.endAccessToIncomplete(whole, capability: .set)) {
        try v.endAccess(x, at: whole, in: m[a], typeLayouts: &m.typeLayouts)
      }
      try v.markInitialized(firstPart, in: m[a], typeLayouts: &m.typeLayouts)
      check(throws: Error.endAccessToIncomplete(whole, capability: .set)) {
        try v.endAccess(x, at: whole, in: m[a], typeLayouts: &m.typeLayouts)
      }
      try v.markInitialized(secondPart, in: m[a], typeLayouts: &m.typeLayouts)
      try v.endAccess(x, at: whole, in: m[a], typeLayouts: &m.typeLayouts)

      let w = try v.beginAccess(k, at: whole, in: m[a], typeLayouts: &m.typeLayouts)
      let s = try v.beginAccess(.sink, at: firstPart, in: m[a], typeLayouts: &m.typeLayouts)
      try v.endAccess(s, at: firstPart, in: m[a], typeLayouts: &m.typeLayouts)
      check(throws: Error.endAccessToIncomplete(whole, capability: k)) {
        try v.endAccess(w, at: whole, in: m[a], typeLayouts: &m.typeLayouts)
      }
    }
  }

  func testMarkInitializedOnAlreadyInitializedObject() throws {
    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(i32)
    let a = p.allocation
    var v = MemorySafetyValidator(a)

    let t = ^TupleType(types: [i8, i8])

    let whole = Memory.Place(allocation: a, offset: 0, type: t)
    let firstPart = Memory.Place(allocation: a, offset: 0, type: i8)
    let secondPart = Memory.Place(allocation: a, offset: 1, type: i8)

    let x = try v.beginAccess(.set, at: whole, in: m[a], typeLayouts: &m.typeLayouts)
    try v.markInitialized(firstPart, in: m[a], typeLayouts: &m.typeLayouts)
    try v.markInitialized(secondPart, in: m[a], typeLayouts: &m.typeLayouts)
    try v.markInitialized(firstPart, in: m[a], typeLayouts: &m.typeLayouts)
    try v.endAccess(x, at: whole, in: m[a], typeLayouts: &m.typeLayouts)

    _ = try v.beginAccess(.let, at: whole, in: m[a], typeLayouts: &m.typeLayouts)
  }

  func testRequireCanReadFromActiveAccesses() throws {
    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(i32)
    let a = p.allocation
    var v = MemorySafetyValidator(a)

    let t = ^TupleType(types: [i8, i8])

    let whole = Memory.Place(allocation: a, offset: 0, type: t)
    let firstPart = Memory.Place(allocation: a, offset: 0, type: i8)
    let secondPart = Memory.Place(allocation: a, offset: 1, type: i8)

    let x = try v.beginAccess(.set, at: whole, in: m[a], typeLayouts: &m.typeLayouts)
    try v.markInitialized(firstPart, in: m[a], typeLayouts: &m.typeLayouts)
    try v.markInitialized(secondPart, in: m[a], typeLayouts: &m.typeLayouts)
    try v.endAccess(x, at: whole, in: m[a], typeLayouts: &m.typeLayouts)

    let w = try v.beginAccess(.sink, at: whole, in: m[a], typeLayouts: &m.typeLayouts)
    try v.requireCanRead(from: whole, using: w, in: m[a], typeLayouts: &m.typeLayouts)
    try v.requireCanRead(from: firstPart, using: w, in: m[a], typeLayouts: &m.typeLayouts)
    try v.requireCanRead(from: secondPart, using: w, in: m[a], typeLayouts: &m.typeLayouts)

    let f = try v.beginAccess(.sink, at: firstPart, in: m[a], typeLayouts: &m.typeLayouts)
    try v.endAccess(f, at: firstPart, in: m[a], typeLayouts: &m.typeLayouts)
    check(throws: Error.readFromIncomplete(whole)) {
      try v.requireCanRead(from: whole, using: w, in: m[a], typeLayouts: &m.typeLayouts)
    }
    check(throws: Error.readFromIncomplete(firstPart)) {
      try v.requireCanRead(from: firstPart, using: w, in: m[a], typeLayouts: &m.typeLayouts)
    }
    try v.requireCanRead(from: secondPart, using: w, in: m[a], typeLayouts: &m.typeLayouts)

    let s = try v.beginAccess(.sink, at: secondPart, in: m[a], typeLayouts: &m.typeLayouts)
    try v.endAccess(s, at: secondPart, in: m[a], typeLayouts: &m.typeLayouts)
    check(throws: Error.readFromIncomplete(whole)) {
      try v.requireCanRead(from: whole, using: w, in: m[a], typeLayouts: &m.typeLayouts)
    }
    check(throws: Error.readFromIncomplete(firstPart)) {
      try v.requireCanRead(from: firstPart, using: w, in: m[a], typeLayouts: &m.typeLayouts)
    }
    check(throws: Error.readFromIncomplete(secondPart)) {
      try v.requireCanRead(from: secondPart, using: w, in: m[a], typeLayouts: &m.typeLayouts)
    }
  }

  func testRequireCanReadFromInactiveAccesses() throws {
    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(i32)
    let a = p.allocation
    var v = MemorySafetyValidator(a)

    let t = ^TupleType(types: [i8, i8])

    let whole = Memory.Place(allocation: a, offset: 0, type: t)
    let firstPart = Memory.Place(allocation: a, offset: 0, type: i8)
    let secondPart = Memory.Place(allocation: a, offset: 1, type: i8)

    let x = try v.beginAccess(.set, at: whole, in: m[a], typeLayouts: &m.typeLayouts)
    try v.markInitialized(firstPart, in: m[a], typeLayouts: &m.typeLayouts)
    try v.markInitialized(secondPart, in: m[a], typeLayouts: &m.typeLayouts)
    try v.endAccess(x, at: whole, in: m[a], typeLayouts: &m.typeLayouts)

    let w = try v.beginAccess(.sink, at: whole, in: m[a], typeLayouts: &m.typeLayouts)
    let w1 = try v.beginAccess(.sink, at: whole, in: m[a], typeLayouts: &m.typeLayouts)
    check(throws: AccessTrackerError.overlappingExclusiveAccess([])) {
      try v.requireCanRead(from: whole, using: w, in: m[a], typeLayouts: &m.typeLayouts)
    }
    check(throws: AccessTrackerError.overlappingExclusiveAccess([firstPart.typedRegion])) {
      try v.requireCanRead(from: firstPart, using: w, in: m[a], typeLayouts: &m.typeLayouts)
    }
    try v.requireCanRead(from: whole, using: w1, in: m[a], typeLayouts: &m.typeLayouts)
    try v.requireCanRead(from: firstPart, using: w1, in: m[a], typeLayouts: &m.typeLayouts)
    let f = try v.beginAccess(.sink, at: firstPart, in: m[a], typeLayouts: &m.typeLayouts)
    check(throws: AccessTrackerError.overlappingExclusiveAccess([firstPart.typedRegion])) {
      try v.requireCanRead(from: firstPart, using: w, in: m[a], typeLayouts: &m.typeLayouts)
    }
    check(throws: AccessTrackerError.overlappingExclusiveAccess([firstPart.typedRegion])) {
      try v.requireCanRead(from: firstPart, using: w1, in: m[a], typeLayouts: &m.typeLayouts)
    }
    try v.requireCanRead(from: firstPart, using: f, in: m[a], typeLayouts: &m.typeLayouts)
    let f1 = try v.beginAccess(.sink, at: firstPart, in: m[a], typeLayouts: &m.typeLayouts)
    check(throws: AccessTrackerError.overlappingExclusiveAccess([firstPart.typedRegion])) {
      try v.requireCanRead(from: firstPart, using: w, in: m[a], typeLayouts: &m.typeLayouts)
    }
    check(throws: AccessTrackerError.overlappingExclusiveAccess([firstPart.typedRegion])) {
      try v.requireCanRead(from: firstPart, using: w1, in: m[a], typeLayouts: &m.typeLayouts)
    }
    check(throws: AccessTrackerError.overlappingExclusiveAccess([firstPart.typedRegion])) {
      try v.requireCanRead(from: firstPart, using: f, in: m[a], typeLayouts: &m.typeLayouts)
    }
    try v.requireCanRead(from: firstPart, using: f1, in: m[a], typeLayouts: &m.typeLayouts)
  }

  func testRequireWriteToUsingActiveAccess() throws {
    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(i32)
    let a = p.allocation
    var v = MemorySafetyValidator(a)

    let t = ^TupleType(types: [i8, i8])

    let whole = Memory.Place(allocation: a, offset: 0, type: t)
    let firstPart = Memory.Place(allocation: a, offset: 0, type: i8)
    let secondPart = Memory.Place(allocation: a, offset: 1, type: i8)

    let x = try v.beginAccess(.set, at: whole, in: m[a], typeLayouts: &m.typeLayouts)
    try v.requireCanWrite(to: whole, using: x, in: m[a], typeLayouts: &m.typeLayouts)
    try v.requireCanWrite(to: firstPart, using: x, in: m[a], typeLayouts: &m.typeLayouts)
    try v.requireCanWrite(to: secondPart, using: x, in: m[a], typeLayouts: &m.typeLayouts)
    try v.markInitialized(firstPart, in: m[a], typeLayouts: &m.typeLayouts)
    try v.requireCanWrite(to: whole, using: x, in: m[a], typeLayouts: &m.typeLayouts)
    try v.requireCanWrite(to: firstPart, using: x, in: m[a], typeLayouts: &m.typeLayouts)
    try v.requireCanWrite(to: secondPart, using: x, in: m[a], typeLayouts: &m.typeLayouts)
    try v.markInitialized(secondPart, in: m[a], typeLayouts: &m.typeLayouts)
    try v.requireCanWrite(to: whole, using: x, in: m[a], typeLayouts: &m.typeLayouts)
    try v.requireCanWrite(to: firstPart, using: x, in: m[a], typeLayouts: &m.typeLayouts)
    try v.requireCanWrite(to: secondPart, using: x, in: m[a], typeLayouts: &m.typeLayouts)
    try v.endAccess(x, at: whole, in: m[a], typeLayouts: &m.typeLayouts)
  }

  func testRequireWriteToUsingInactiveAccess() throws {
    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(i32)
    let a = p.allocation
    var v = MemorySafetyValidator(a)

    let t = ^TupleType(types: [i8, i8])

    let whole = Memory.Place(allocation: a, offset: 0, type: t)
    let firstPart = Memory.Place(allocation: a, offset: 0, type: i8)
    let secondPart = Memory.Place(allocation: a, offset: 1, type: i8)

    let x = try v.beginAccess(.set, at: whole, in: m[a], typeLayouts: &m.typeLayouts)
    try v.markInitialized(firstPart, in: m[a], typeLayouts: &m.typeLayouts)
    try v.markInitialized(secondPart, in: m[a], typeLayouts: &m.typeLayouts)
    try v.endAccess(x, at: whole, in: m[a], typeLayouts: &m.typeLayouts)

    let w = try v.beginAccess(.sink, at: whole, in: m[a], typeLayouts: &m.typeLayouts)
    let w1 = try v.beginAccess(.sink, at: whole, in: m[a], typeLayouts: &m.typeLayouts)
    check(throws: AccessTrackerError.overlappingExclusiveAccess([])) {
      try v.requireCanWrite(to: whole, using: w, in: m[a], typeLayouts: &m.typeLayouts)
    }
    check(throws: AccessTrackerError.overlappingExclusiveAccess([firstPart.typedRegion])) {
      try v.requireCanWrite(to: firstPart, using: w, in: m[a], typeLayouts: &m.typeLayouts)
    }
    try v.requireCanWrite(to: whole, using: w1, in: m[a], typeLayouts: &m.typeLayouts)
    try v.requireCanWrite(to: firstPart, using: w1, in: m[a], typeLayouts: &m.typeLayouts)
    let f = try v.beginAccess(.sink, at: firstPart, in: m[a], typeLayouts: &m.typeLayouts)
    check(throws: AccessTrackerError.overlappingExclusiveAccess([firstPart.typedRegion])) {
      try v.requireCanWrite(to: firstPart, using: w, in: m[a], typeLayouts: &m.typeLayouts)
    }
    check(throws: AccessTrackerError.overlappingExclusiveAccess([firstPart.typedRegion])) {
      try v.requireCanWrite(to: firstPart, using: w1, in: m[a], typeLayouts: &m.typeLayouts)
    }
    try v.requireCanWrite(to: firstPart, using: f, in: m[a], typeLayouts: &m.typeLayouts)
    let f1 = try v.beginAccess(.sink, at: firstPart, in: m[a], typeLayouts: &m.typeLayouts)
    check(throws: AccessTrackerError.overlappingExclusiveAccess([firstPart.typedRegion])) {
      try v.requireCanWrite(to: firstPart, using: w, in: m[a], typeLayouts: &m.typeLayouts)
    }
    check(throws: AccessTrackerError.overlappingExclusiveAccess([firstPart.typedRegion])) {
      try v.requireCanWrite(to: firstPart, using: w1, in: m[a], typeLayouts: &m.typeLayouts)
    }
    check(throws: AccessTrackerError.overlappingExclusiveAccess([firstPart.typedRegion])) {
      try v.requireCanWrite(to: firstPart, using: f, in: m[a], typeLayouts: &m.typeLayouts)
    }
    try v.requireCanWrite(to: firstPart, using: f1, in: m[a], typeLayouts: &m.typeLayouts)
  }

}

private extension Memory.Place {
  var typedRegion: Memory.Allocation.TypedRegion {
    .init(offset: offset, type: type)
  }
}
