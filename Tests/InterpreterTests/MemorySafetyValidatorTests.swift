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
  typealias AccessValidatorError = AccessValidator<Memory.Allocation.TypedRegion>.Error

  /// Deinitializes `r` in `a` on `validator` assuming allocations uses layouts
  /// from `typeLayouts`.
  func deinitialize(
    _ r: Memory.Allocation.TypedRegion,
    in a: Memory.Allocation, validator: inout MemorySafetyValidator,
    typeLayouts: inout TypeLayoutCache
  ) throws {
    let t = try validator.begin(.sink, to: r, in: a, typeLayouts: &typeLayouts)
    try validator.end(t, in: a, typeLayouts: &typeLayouts)
  }

  func testBindingTypeToInvalidMemoryRegion() throws {
    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(i32)
    let a = p.allocation
    var v = MemorySafetyValidator(a)

    check(throws: Memory.Error.alignment(p.address + 1, for: m.typeLayouts[i16])) {
      _ = try v.begin(
        .set, to: .init(offset: 1, type: i16), in: m[a], typeLayouts: &m.typeLayouts)
    }

    check(throws: Memory.Error.bounds(p.address, for: m.typeLayouts[i64], allocationSize: 4)) {
      _ = try v.begin(
        .set, to: .init(offset: 0, type: i64), in: m[a], typeLayouts: &m.typeLayouts)
    }

    _ = try v.begin(
      .set, to: .init(offset: 2, type: i16), in: m[a], typeLayouts: &m.typeLayouts)
    check(throws: ReservedTypeRegions.Error.regionAlreadyBound(to: i16)) {
      _ = try v.begin(
        .set, to: .init(offset: 0, type: i32), in: m[a], typeLayouts: &m.typeLayouts)
    }
  }

  func testCreatingNonSetAccessToInCompleteRegion() throws {
    for k in nonSetAccesses {
      var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
      let p = m.allocate(i32)
      let a = p.allocation
      var v = MemorySafetyValidator(a)

      let t = ^TupleType(types: [i8, i8])

      let whole = Memory.Allocation.TypedRegion(offset: 0, type: t)
      let firstPart = Memory.Allocation.TypedRegion(offset: 0, type: i8)
      let secondPart = Memory.Allocation.TypedRegion(offset: 1, type: i8)

      // No parts are initialized
      _ = try v.begin(.set, to: whole, in: m[a], typeLayouts: &m.typeLayouts)
      check(throws: Error.accessToIncomplete(place(whole, in: a), effect: k)) {
        _ = try v.begin(k, to: whole, in: m[a], typeLayouts: &m.typeLayouts)
      }
      check(throws: Error.accessToIncomplete(place(firstPart, in: a), effect: k)) {
        _ = try v.begin(k, to: firstPart, in: m[a], typeLayouts: &m.typeLayouts)
      }
      check(throws: Error.accessToIncomplete(place(secondPart, in: a), effect: k)) {
        _ = try v.begin(k, to: secondPart, in: m[a], typeLayouts: &m.typeLayouts)
      }

      // First part is initialized
      try v.markInitialized(firstPart, in: m[a], typeLayouts: &m.typeLayouts)
      check(throws: Error.accessToIncomplete(place(whole, in: a), effect: k)) {
        _ = try v.begin(k, to: whole, in: m[a], typeLayouts: &m.typeLayouts)
      }
      let f = try v.begin(k, to: firstPart, in: m[a], typeLayouts: &m.typeLayouts)
      try v.end(f, in: m[a], typeLayouts: &m.typeLayouts)
      check(throws: Error.accessToIncomplete(place(secondPart, in: a), effect: k)) {
        _ = try v.begin(k, to: secondPart, in: m[a], typeLayouts: &m.typeLayouts)
      }

      // Both parts are initialized
      if k == .sink { try v.markInitialized(firstPart, in: m[a], typeLayouts: &m.typeLayouts) }
      try v.markInitialized(secondPart, in: m[a], typeLayouts: &m.typeLayouts)
      _ = try v.begin(k, to: whole, in: m[a], typeLayouts: &m.typeLayouts)
      _ = try v.begin(k, to: firstPart, in: m[a], typeLayouts: &m.typeLayouts)
      _ = try v.begin(k, to: secondPart, in: m[a], typeLayouts: &m.typeLayouts)
    }
  }

  func testCreatingSetAccessToPartiallyInitializedRegion() throws {
    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(i32)
    let a = p.allocation
    var v = MemorySafetyValidator(a)

    let t = ^TupleType(types: [i8, i8])

    let whole = Memory.Allocation.TypedRegion(offset: 0, type: t)
    let firstPart = Memory.Allocation.TypedRegion(offset: 0, type: i8)
    let secondPart = Memory.Allocation.TypedRegion(offset: 1, type: i8)

    _ = try v.begin(.set, to: whole, in: m[a], typeLayouts: &m.typeLayouts)

    try v.markInitialized(firstPart, in: m[a], typeLayouts: &m.typeLayouts)
    check(throws: Error.setAccessToPartiallyComplete(place(firstPart, in: a))) {
      _ = try v.begin(.set, to: firstPart, in: m[a], typeLayouts: &m.typeLayouts)
    }
    check(throws: Error.setAccessToPartiallyComplete(place(whole, in: a))) {
      _ = try v.begin(.set, to: whole, in: m[a], typeLayouts: &m.typeLayouts)
    }

    try v.markInitialized(secondPart, in: m[a], typeLayouts: &m.typeLayouts)
    check(throws: Error.setAccessToPartiallyComplete(place(firstPart, in: a))) {
      _ = try v.begin(.set, to: firstPart, in: m[a], typeLayouts: &m.typeLayouts)
    }
    check(throws: Error.setAccessToPartiallyComplete(place(secondPart, in: a))) {
      _ = try v.begin(.set, to: secondPart, in: m[a], typeLayouts: &m.typeLayouts)
    }
    check(throws: Error.setAccessToPartiallyComplete(place(whole, in: a))) {
      _ = try v.begin(.set, to: whole, in: m[a], typeLayouts: &m.typeLayouts)
    }
  }

  func testEndingAccessOnCompleteObjects() throws {
    for k in nonSetAccesses {
      var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
      let p = m.allocate(i32)
      let a = p.allocation
      var v = MemorySafetyValidator(a)

      let t = ^TupleType(types: [i8, i8])

      let whole = Memory.Allocation.TypedRegion(offset: 0, type: t)
      let firstPart = Memory.Allocation.TypedRegion(offset: 0, type: i8)
      let secondPart = Memory.Allocation.TypedRegion(offset: 1, type: i8)

      let x = try v.begin(.set, to: whole, in: m[a], typeLayouts: &m.typeLayouts)
      try v.markInitialized(firstPart, in: m[a], typeLayouts: &m.typeLayouts)
      try v.markInitialized(secondPart, in: m[a], typeLayouts: &m.typeLayouts)
      try v.end(x, in: m[a], typeLayouts: &m.typeLayouts)

      let w = try v.begin(k, to: whole, in: m[a], typeLayouts: &m.typeLayouts)
      try v.end(w, in: m[a], typeLayouts: &m.typeLayouts)
      if k == .sink {
        try v.markInitialized(firstPart, in: m[a], typeLayouts: &m.typeLayouts)
        try v.markInitialized(secondPart, in: m[a], typeLayouts: &m.typeLayouts)
      }
      let f = try v.begin(k, to: firstPart, in: m[a], typeLayouts: &m.typeLayouts)
      try v.end(f, in: m[a], typeLayouts: &m.typeLayouts)
      let s = try v.begin(k, to: secondPart, in: m[a], typeLayouts: &m.typeLayouts)
      try v.end(s, in: m[a], typeLayouts: &m.typeLayouts)
    }
  }

  func testEndingAccessOnIncompleteObjects() throws {
    for k in [.let, .inout] as [AccessEffect] {
      var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
      let p = m.allocate(i32)
      let a = p.allocation
      var v = MemorySafetyValidator(a)

      let t = ^TupleType(types: [i8, i8])

      let whole = Memory.Allocation.TypedRegion(offset: 0, type: t)
      let firstPart = Memory.Allocation.TypedRegion(offset: 0, type: i8)
      let secondPart = Memory.Allocation.TypedRegion(offset: 1, type: i8)

      let x = try v.begin(.set, to: whole, in: m[a], typeLayouts: &m.typeLayouts)
      check(throws: Error.endAccessToIncomplete(place(whole, in: a), effect: .set)) {
        try v.end(x, in: m[a], typeLayouts: &m.typeLayouts)
      }
      try v.markInitialized(firstPart, in: m[a], typeLayouts: &m.typeLayouts)
      check(throws: Error.endAccessToIncomplete(place(whole, in: a), effect: .set)) {
        try v.end(x, in: m[a], typeLayouts: &m.typeLayouts)
      }
      try v.markInitialized(secondPart, in: m[a], typeLayouts: &m.typeLayouts)
      try v.end(x, in: m[a], typeLayouts: &m.typeLayouts)

      let w = try v.begin(k, to: whole, in: m[a], typeLayouts: &m.typeLayouts)
      let s = try v.begin(.sink, to: firstPart, in: m[a], typeLayouts: &m.typeLayouts)
      try v.end(s, in: m[a], typeLayouts: &m.typeLayouts)
      check(throws: Error.endAccessToIncomplete(place(whole, in: a), effect: k)) {
        try v.end(w, in: m[a], typeLayouts: &m.typeLayouts)
      }
    }
  }

  func testMarkInitializedOnAlreadyInitializedObject() throws {
    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(i32)
    let a = p.allocation
    var v = MemorySafetyValidator(a)

    let t = ^TupleType(types: [i8, i8])

    let whole = Memory.Allocation.TypedRegion(offset: 0, type: t)
    let firstPart = Memory.Allocation.TypedRegion(offset: 0, type: i8)
    let secondPart = Memory.Allocation.TypedRegion(offset: 1, type: i8)

    let x = try v.begin(.set, to: whole, in: m[a], typeLayouts: &m.typeLayouts)
    try v.markInitialized(firstPart, in: m[a], typeLayouts: &m.typeLayouts)
    try v.markInitialized(secondPart, in: m[a], typeLayouts: &m.typeLayouts)
    try v.markInitialized(firstPart, in: m[a], typeLayouts: &m.typeLayouts)
    try v.end(x, in: m[a], typeLayouts: &m.typeLayouts)

    _ = try v.begin(.let, to: whole, in: m[a], typeLayouts: &m.typeLayouts)
  }

  func testRequireCanReadFromActiveAccesses() throws {
    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(i32)
    let a = p.allocation
    var v = MemorySafetyValidator(a)

    let t = ^TupleType(types: [i8, i8])

    let whole = Memory.Allocation.TypedRegion(offset: 0, type: t)
    let firstPart = Memory.Allocation.TypedRegion(offset: 0, type: i8)
    let secondPart = Memory.Allocation.TypedRegion(offset: 1, type: i8)

    let x = try v.begin(.set, to: whole, in: m[a], typeLayouts: &m.typeLayouts)
    try v.markInitialized(firstPart, in: m[a], typeLayouts: &m.typeLayouts)
    try v.markInitialized(secondPart, in: m[a], typeLayouts: &m.typeLayouts)
    try v.end(x, in: m[a], typeLayouts: &m.typeLayouts)

    let _ = try v.begin(.sink, to: whole, in: m[a], typeLayouts: &m.typeLayouts)
    let w = try v.begin(.let, to: whole, in: m[a], typeLayouts: &m.typeLayouts)
    let f = try v.begin(.let, to: firstPart, in: m[a], typeLayouts: &m.typeLayouts)
    let s = try v.begin(.let, to: secondPart, in: m[a], typeLayouts: &m.typeLayouts)
    try v.requireCanRead(from: w, in: m[a], typeLayouts: &m.typeLayouts)
    try v.requireCanRead(from: f, in: m[a], typeLayouts: &m.typeLayouts)
    try v.requireCanRead(from: s, in: m[a], typeLayouts: &m.typeLayouts)

    try deinitialize(firstPart, in: m[a], validator: &v, typeLayouts: &m.typeLayouts)
    check(throws: Error.readFromIncomplete(place(whole, in: a))) {
      try v.requireCanRead(from: w, in: m[a], typeLayouts: &m.typeLayouts)
    }
    check(throws: Error.readFromIncomplete(place(firstPart, in: a))) {
      try v.requireCanRead(from: f, in: m[a], typeLayouts: &m.typeLayouts)
    }
    try v.requireCanRead(from: s, in: m[a], typeLayouts: &m.typeLayouts)

    try deinitialize(secondPart, in: m[a], validator: &v, typeLayouts: &m.typeLayouts)
    check(throws: Error.readFromIncomplete(place(whole, in: a))) {
      try v.requireCanRead(from: w, in: m[a], typeLayouts: &m.typeLayouts)
    }
    check(throws: Error.readFromIncomplete(place(firstPart, in: a))) {
      try v.requireCanRead(from: f, in: m[a], typeLayouts: &m.typeLayouts)
    }
    check(throws: Error.readFromIncomplete(place(secondPart, in: a))) {
      try v.requireCanRead(from: s, in: m[a], typeLayouts: &m.typeLayouts)
    }
  }

  func testRequireCanReadFromInactiveAccesses() throws {
    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(i32)
    let a = p.allocation
    var v = MemorySafetyValidator(a)

    let t = ^TupleType(types: [i8, i8])

    let whole = Memory.Allocation.TypedRegion(offset: 0, type: t)
    let firstPart = Memory.Allocation.TypedRegion(offset: 0, type: i8)
    let secondPart = Memory.Allocation.TypedRegion(offset: 1, type: i8)

    let x = try v.begin(.set, to: whole, in: m[a], typeLayouts: &m.typeLayouts)
    try v.markInitialized(firstPart, in: m[a], typeLayouts: &m.typeLayouts)
    try v.markInitialized(secondPart, in: m[a], typeLayouts: &m.typeLayouts)
    try v.end(x, in: m[a], typeLayouts: &m.typeLayouts)

    let w = try v.begin(.sink, to: whole, in: m[a], typeLayouts: &m.typeLayouts)
    let w1 = try v.begin(.sink, to: whole, in: m[a], typeLayouts: &m.typeLayouts)
    check(throws: AccessValidatorError.overlappingExclusiveAccess([])) {
      try v.requireCanRead(from: w, in: m[a], typeLayouts: &m.typeLayouts)
    }
    try v.requireCanRead(from: w1, in: m[a], typeLayouts: &m.typeLayouts)
    let f = try v.begin(.sink, to: firstPart, in: m[a], typeLayouts: &m.typeLayouts)
    try v.requireCanRead(from: f, in: m[a], typeLayouts: &m.typeLayouts)
    let f1 = try v.begin(.sink, to: firstPart, in: m[a], typeLayouts: &m.typeLayouts)
    check(throws: AccessValidatorError.overlappingExclusiveAccess([firstPart])) {
      try v.requireCanRead(from: f, in: m[a], typeLayouts: &m.typeLayouts)
    }
    try v.requireCanRead(from: f1, in: m[a], typeLayouts: &m.typeLayouts)
    try v.end(f1, in: m[a], typeLayouts: &m.typeLayouts)
  }

  func testRequireWriteToUsingActiveAccess() throws {
    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(i32)
    let a = p.allocation
    var v = MemorySafetyValidator(a)

    let t = ^TupleType(types: [i8, i8])

    let whole = Memory.Allocation.TypedRegion(offset: 0, type: t)
    let firstPart = Memory.Allocation.TypedRegion(offset: 0, type: i8)
    let secondPart = Memory.Allocation.TypedRegion(offset: 1, type: i8)

    let w = try v.begin(.set, to: whole, in: m[a], typeLayouts: &m.typeLayouts)
    try v.requireCanWrite(to: w, in: m[a], typeLayouts: &m.typeLayouts)
    try v.markInitialized(firstPart, in: m[a], typeLayouts: &m.typeLayouts)
    try v.markInitialized(secondPart, in: m[a], typeLayouts: &m.typeLayouts)
    let f = try v.begin(.inout, to: firstPart, in: m[a], typeLayouts: &m.typeLayouts)
    let s = try v.begin(.inout, to: secondPart, in: m[a], typeLayouts: &m.typeLayouts)
    try v.requireCanWrite(to: f, in: m[a], typeLayouts: &m.typeLayouts)
    try v.requireCanWrite(to: s, in: m[a], typeLayouts: &m.typeLayouts)
    try v.end(f, in: m[a], typeLayouts: &m.typeLayouts)
    try v.end(s, in: m[a], typeLayouts: &m.typeLayouts)
    try v.end(w, in: m[a], typeLayouts: &m.typeLayouts)
  }

  func testRequireWriteToUsingInactiveAccess() throws {
    var m = Memory(typesIn: TypedProgram.empty, for: UnrealABI())
    let p = m.allocate(i32)
    let a = p.allocation
    var v = MemorySafetyValidator(a)

    let t = ^TupleType(types: [i8, i8])

    let whole = Memory.Allocation.TypedRegion(offset: 0, type: t)
    let firstPart = Memory.Allocation.TypedRegion(offset: 0, type: i8)
    let secondPart = Memory.Allocation.TypedRegion(offset: 1, type: i8)

    let x = try v.begin(.set, to: whole, in: m[a], typeLayouts: &m.typeLayouts)
    try v.markInitialized(firstPart, in: m[a], typeLayouts: &m.typeLayouts)
    try v.markInitialized(secondPart, in: m[a], typeLayouts: &m.typeLayouts)
    try v.end(x, in: m[a], typeLayouts: &m.typeLayouts)

    let w = try v.begin(.sink, to: whole, in: m[a], typeLayouts: &m.typeLayouts)
    let w1 = try v.begin(.sink, to: whole, in: m[a], typeLayouts: &m.typeLayouts)
    check(throws: AccessValidatorError.overlappingExclusiveAccess([])) {
      try v.requireCanWrite(to: w, in: m[a], typeLayouts: &m.typeLayouts)
    }
    try v.requireCanWrite(to: w1, in: m[a], typeLayouts: &m.typeLayouts)
    let f = try v.begin(.sink, to: firstPart, in: m[a], typeLayouts: &m.typeLayouts)
    try v.requireCanWrite(to: f, in: m[a], typeLayouts: &m.typeLayouts)
    let f1 = try v.begin(.sink, to: firstPart, in: m[a], typeLayouts: &m.typeLayouts)
    check(throws: AccessValidatorError.overlappingExclusiveAccess([firstPart])) {
      try v.requireCanWrite(to: f, in: m[a], typeLayouts: &m.typeLayouts)
    }
    try v.requireCanWrite(to: f1, in: m[a], typeLayouts: &m.typeLayouts)
  }

  /// Returns `Place` to `r` in `a`.
  func place(_ r: Memory.Allocation.TypedRegion, in a: Memory.Allocation.ID) -> Memory.Place {
    .init(allocation: a, offset: r.offset, type: r.type)
  }

}

private extension Memory.Place {
  var typedRegion: Memory.Allocation.TypedRegion {
    .init(offset: offset, type: type)
  }
}
