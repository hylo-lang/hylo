import Interpreter
import TestUtils
import Utils
import XCTest

extension Character: Regular {}

final class AccessStackForestTests: XCTestCase {

  private typealias Error = AccessError<Character>

  func testAddingNewLetAccess() throws {
    var t = AccessStackForest<Character>()
    _ = try t.add(.let, at: ["a"], derivedFrom: nil)
    _ = try t.add(.let, at: ["a"], derivedFrom: nil)
    _ = try t.add(.let, at: ["b"], derivedFrom: nil)
  }

  func testAddingNewNonLetAccess() throws {
    let accesses: [AccessKind] = [.inout, .set, .sink, .let]
    for p in accesses {
      for a in accesses.dropLast() {
        var t = AccessStackForest<Character>()
        _ = try t.add(p, at: ["a"], derivedFrom: nil)
        _ = try t.add(p, at: ["b"], derivedFrom: nil)  // Adding new access to different tree is fine.
        check(throws: Error.overlappingMutableAccessExists(for: "a")) {
          _ = try t.add(a, at: ["a"], derivedFrom: nil)
        }
      }
    }
  }

  func testAddingDerivedLetAccess() throws {
    var t = AccessStackForest<Character>()
    let a1 = try t.add(.let, at: ["a"], derivedFrom: nil)

    let invalidAccess = Access(kind: .let)
    check(throws: Error.accessNotFound(invalidAccess)) {
      _ = try t.add(.let, at: ["a"], derivedFrom: invalidAccess)
    }

    let a2 = try t.add(.let, at: ["a"], derivedFrom: a1)
    let a3 = try t.add(.let, at: ["a", "b", "c"], derivedFrom: a1)
    _ = try t.add(.let, at: ["a", "b", "c"], derivedFrom: a1)
    _ = try t.add(.let, at: ["a", "b", "c"], derivedFrom: a2)
    _ = try t.add(.let, at: ["a", "b", "c"], derivedFrom: a3)

    for a in [.inout, .sink] as [AccessKind] {
      var t = AccessStackForest<Character>()
      let a4 = try t.add(a, at: ["p"], derivedFrom: nil)
      let a5 = try t.add(a, at: ["p"], derivedFrom: a4)
      check(throws: Error.canNotDerive(.let, for: "p", from: a4, at: "p")) {
        _ = try t.add(.let, at: ["p"], derivedFrom: a4)
      }
      let a6 = try t.add(.let, at: ["p"], derivedFrom: a5)
      try t.end(a6, at: ["p"])
      try t.end(a5, at: ["p"])
      _ = try t.add(.let, at: ["p"], derivedFrom: a4)

      let a7 = try t.add(a, at: ["x"], derivedFrom: nil)
      let a8 = try t.add(a, at: ["x", "y"], derivedFrom: a7)
      check(throws: Error.canNotDerive(.let, for: "y", from: a7, at: "x")) {
        _ = try t.add(.let, at: ["x", "y"], derivedFrom: a7)
      }
      let a9 = try t.add(.let, at: ["x", "y"], derivedFrom: a8)
      try t.end(a9, at: ["x", "y"])
      try t.end(a8, at: ["x", "y"])
      _ = try t.add(.let, at: ["x", "y"], derivedFrom: a7)
    }
  }

  func testAddingDerivedNonLetAccess() throws {
    let accesses: [AccessKind] = [.set, .sink, .inout]
    for a in accesses {
      var t = AccessStackForest<Character>()
      let a1 = try t.add(a, at: ["a"], derivedFrom: nil)

      let invalidAccess = Access(kind: a)
      check(throws: Error.accessNotFound(invalidAccess)) {
        _ = try t.add(a, at: ["a"], derivedFrom: invalidAccess)
      }

      let a2 = try t.add(a, at: ["a"], derivedFrom: a1)
      check(throws: Error.canNotDerive(a, for: "a", from: a1, at: "a")) {
        _ = try t.add(a, at: ["a"], derivedFrom: a1)
      }
      let a3 = try t.add(a, at: ["a"], derivedFrom: a2)
      check(throws: Error.canNotDerive(a, for: "c", from: a2, at: "a")) {
        _ = try t.add(a, at: ["a", "b", "c"], derivedFrom: a2)
      }
      let a4 = try t.add(a, at: ["a", "b", "c"], derivedFrom: a3)
      try t.end(a4, at: ["a", "b", "c"])
      try t.end(a3, at: ["a"])
      let a5 = try t.add(a, at: ["a", "b", "c"], derivedFrom: a2)
      try t.end(a5, at: ["a", "b", "c"])
      try t.end(a2, at: ["a"])
      _ = try t.add(a, at: ["a"], derivedFrom: a1)
    }
  }

  func testDerivationKindCompatibility() throws {
    let r: [(AccessKind, canBeDerivedFrom: [AccessKind], canNotBeDerivedFrom: [AccessKind])] = [
      (.let, canBeDerivedFrom: [.let, .inout, .sink], canNotBeDerivedFrom: [.set]),
      (.set, canBeDerivedFrom: [.set, .inout, .sink], canNotBeDerivedFrom: [.let]),
      (.inout, canBeDerivedFrom: [.inout, .sink], canNotBeDerivedFrom: [.let, .set]),
      (.sink, canBeDerivedFrom: [.inout, .sink], canNotBeDerivedFrom: [.let, .set]),
    ]

    for (a, d, nd) in r {
      for p in d {
        var t = AccessStackForest<Character>()
        let a1 = try t.add(p, at: ["a"], derivedFrom: nil)
        _ = try t.add(a, at: ["a"], derivedFrom: a1)
      }

      for p in nd {
        var t = AccessStackForest<Character>()
        let a1 = try t.add(p, at: ["a"], derivedFrom: nil)
        check(throws: Error.canNotDerive(a, for: "a", from: a1, at: "a")) {
          _ = try t.add(a, at: ["a"], derivedFrom: a1)
        }
      }
    }
  }

}
