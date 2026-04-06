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

    let other = try t.add(.let, at: ["a", "x"], derivedFrom: nil)
    check(throws: Error.accessNotFound(other, inPathTo: "a")) {
      _ = try t.add(.let, at: ["a"], derivedFrom: other)
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
      check(throws: Error.cannotDerive(.let, for: "p", from: a4, at: "p", dueToAccess: a5, on: "p"))
      {
        _ = try t.add(.let, at: ["p"], derivedFrom: a4)
      }
      let a6 = try t.add(.let, at: ["p"], derivedFrom: a5)
      try t.end(a6, at: ["p"])
      try t.end(a5, at: ["p"])
      _ = try t.add(.let, at: ["p"], derivedFrom: a4)

      let a7 = try t.add(a, at: ["x"], derivedFrom: nil)
      let a8 = try t.add(a, at: ["x", "y"], derivedFrom: a7)
      check(throws: Error.cannotDerive(.let, for: "y", from: a7, at: "x", dueToAccess: a8, on: "y"))
      {
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

      let other = Access(kind: a)
      check(throws: Error.accessNotFound(other, inPathTo: "a")) {
        _ = try t.add(a, at: ["a"], derivedFrom: other)
      }

      let a2 = try t.add(a, at: ["a"], derivedFrom: a1)
      check(throws: Error.cannotDerive(a, for: "a", from: a1, at: "a", dueToAccess: a2, on: "a")) {
        _ = try t.add(a, at: ["a"], derivedFrom: a1)
      }
      let a3 = try t.add(a, at: ["a"], derivedFrom: a2)
      check(throws: Error.cannotDerive(a, for: "c", from: a2, at: "a", dueToAccess: a3, on: "a")) {
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
    let r: [(AccessKind, canBeDerivedFrom: [AccessKind], cannotBeDerivedFrom: [AccessKind])] = [
      (.let, canBeDerivedFrom: [.let, .inout, .sink], cannotBeDerivedFrom: [.set]),
      (.set, canBeDerivedFrom: [.set, .inout, .sink], cannotBeDerivedFrom: [.let]),
      (.inout, canBeDerivedFrom: [.inout, .sink], cannotBeDerivedFrom: [.let, .set]),
      (.sink, canBeDerivedFrom: [.inout, .sink], cannotBeDerivedFrom: [.let, .set]),
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
        check(throws: Error.incompatibleDerivation(of: a, from: p)) {
          _ = try t.add(a, at: ["a"], derivedFrom: a1)
        }
      }
    }
  }

  func testAddingLetAccessBlockedByDescendants() throws {
    for a in [.inout, .sink] as [AccessKind] {
      var t = AccessStackForest<Character>()
      let a1 = try t.add(a, at: ["a"], derivedFrom: nil)
      let a2 = try t.add(a, at: ["a", "b"], derivedFrom: a1)
      let a3 = try t.add(a, at: ["a", "c", "d"], derivedFrom: a1)
      check(throws: Error.overlappingMutableAccessExists(for: "a")) {
        _ = try t.add(.let, at: ["a"], derivedFrom: a1)
      }
      try t.end(a2, at: ["a", "b"])
      check(throws: Error.overlappingMutableAccessExists(for: "a")) {
        _ = try t.add(a, at: ["a"], derivedFrom: a1)
      }
      try t.end(a3, at: ["a", "c", "d"])
      _ = try t.add(.let, at: ["a"], derivedFrom: a1)
    }

    var t = AccessStackForest<Character>()
    let a1 = try t.add(.let, at: ["a"], derivedFrom: nil)
    _ = try t.add(.let, at: ["a", "b"], derivedFrom: a1)
    _ = try t.add(.let, at: ["a", "c", "d"], derivedFrom: a1)
    _ = try t.add(.let, at: ["a"], derivedFrom: a1)
  }

  func testAddingNonLetAccessBlockedByDescendants() throws {
    for a in [.set, .inout, .sink] as [AccessKind] {
      var t = AccessStackForest<Character>()
      let a1 = try t.add(a, at: ["a"], derivedFrom: nil)
      let a2 = try t.add(a, at: ["a", "b"], derivedFrom: a1)
      let a3 = try t.add(a, at: ["a", "c", "d"], derivedFrom: a1)
      check(throws: Error.overlappingMutableAccessExists(for: "a")) {
        _ = try t.add(a, at: ["a"], derivedFrom: a1)
      }
      try t.end(a2, at: ["a", "b"])
      check(throws: Error.overlappingMutableAccessExists(for: "a")) {
        _ = try t.add(a, at: ["a"], derivedFrom: a1)
      }
      try t.end(a3, at: ["a", "c", "d"])
      _ = try t.add(a, at: ["a"], derivedFrom: a1)
    }
  }

  func testEndingLetAccess() throws {
    var t = AccessStackForest<Character>()
    let r = try t.add(.inout, at: ["r"], derivedFrom: nil)
    let a1 = try t.add(.let, at: ["r", "a"], derivedFrom: r)
    let a2 = try t.add(.let, at: ["r", "a", "b"], derivedFrom: a1)
    let a3 = try t.add(.let, at: ["r", "a", "c", "d"], derivedFrom: a1)
    try t.end(a2, at: ["r", "a", "b"])  // can end out of order
    try t.end(a1, at: ["r", "a"])
    try t.end(a3, at: ["r", "a", "c", "d"])
    let a4 = try t.add(.inout, at: ["r"], derivedFrom: r)
    try t.end(a4, at: ["r"])
    try t.end(r, at: ["r"])
  }

  func testEndingNonLetAccess() throws {
    for a in [.set, .sink, .inout] as [AccessKind] {
      var t = AccessStackForest<Character>()
      let r = try t.add(a, at: ["r"], derivedFrom: nil)
      let a1 = try t.add(a, at: ["r", "a", "b"], derivedFrom: r)
      let a2 = try t.add(a, at: ["r", "a", "b", "c"], derivedFrom: a1)
      let a3 = try t.add(a, at: ["r", "a", "b", "d", "e"], derivedFrom: a1)
      check(throws: Error.activeDerivedAccessExists(for: a1, at: "b")) {
        try t.end(a1, at: ["r", "a", "b"])
      }
      try t.end(a2, at: ["r", "a", "b", "c"])
      check(throws: Error.activeDerivedAccessExists(for: a1, at: "b")) {
        try t.end(a1, at: ["r", "a", "b"])
      }
      try t.end(a3, at: ["r", "a", "b", "d", "e"])
      try t.end(a1, at: ["r", "a", "b"])
      let a4 = try t.add(a, at: ["r"], derivedFrom: r)
      try t.end(a4, at: ["r"])
      try t.end(r, at: ["r"])

      let a5 = try t.add(a, at: ["x"], derivedFrom: nil)
      let a6 = try t.add(a, at: ["x"], derivedFrom: a5)
      check(throws: Error.activeDerivedAccessExists(for: a5, at: "x")) {
        try t.end(a5, at: ["x"])
      }
      try t.end(a6, at: ["x"])
      try t.end(a5, at: ["x"])
    }
  }

  func testEndingInvalidAccess() throws {
    var t = AccessStackForest<Character>()
    let a1 = try t.add(.let, at: ["a"], derivedFrom: nil)
    let a2 = try t.add(.let, at: ["a", "b"], derivedFrom: nil)
    check(throws: Error.pathNotFound(["a", "c"])) {
      try t.end(a2, at: ["a", "c"])
    }
    check(throws: Error.accessNotFound(a1, inPathTo: "b")) {
      try t.end(a1, at: ["a", "b"])
    }
  }

  func testUsingLetAccess() throws {
    var t = AccessStackForest<Character>()
    let a1 = try t.add(.inout, at: ["a"], derivedFrom: nil)
    let a2 = try t.add(.let, at: ["a"], derivedFrom: a1)
    let a3 = try t.add(.let, at: ["a"], derivedFrom: a2)
    let a4 = try t.add(.let, at: ["a", "b"], derivedFrom: a1)
    let a5 = try t.add(.let, at: ["a", "b"], derivedFrom: a4)
    try t.requireIsUsable(a2, at: ["a"])
    try t.requireIsUsable(a3, at: ["a"])
    try t.requireIsUsable(a4, at: ["a", "b"])
    try t.requireIsUsable(a5, at: ["a", "b"])
    check(throws: Error.pathNotFound(["a", "c"])) {
      try t.requireIsUsable(a5, at: ["a", "c"])
    }
    check(throws: Error.accessNotFound(a2, inPathTo: "b")) {
      try t.requireIsUsable(a2, at: ["a", "b"])
    }
  }

  func testUsingNonLetAccess() throws {
    for a in [.set, .sink, .inout] as [AccessKind] {
      var t = AccessStackForest<Character>()
      let a1 = try t.add(a, at: ["a"], derivedFrom: nil)
      let a2 = try t.add(a, at: ["a"], derivedFrom: a1)
      let a3 = try t.add(a, at: ["a"], derivedFrom: a2)
      let a4 = try t.add(a, at: ["a", "b"], derivedFrom: a3)
      let a5 = try t.add(a, at: ["a", "b"], derivedFrom: a4)

      check(throws: Error.pathNotFound(["a", "c"])) {
        try t.requireIsUsable(a5, at: ["a", "c"])
      }
      check(throws: Error.accessNotFound(a2, inPathTo: "b")) {
        try t.requireIsUsable(a2, at: ["a", "b"])
      }

      var accessPaths: [(Access, [Character])] = [
        (a1, ["a"]),
        (a2, ["a"]),
        (a3, ["a"]),
        (a4, ["a", "b"]),
        (a5, ["a", "b"]),
      ]

      func checkCanNotUse(till i: Int) throws {
        for (a, p) in accessPaths[0..<i] {
          check(throws: Error.activeDerivedAccessExists(for: a, at: p.last!)) {
            try t.requireIsUsable(a, at: p)
          }
        }
        try t.requireIsUsable(accessPaths[i].0, at: accessPaths[i].1)
      }

      while !accessPaths.isEmpty {
        try checkCanNotUse(till: accessPaths.count - 1)
        let (a, p) = accessPaths.popLast()!
        try t.end(a, at: p)
      }

      let a6 = try t.add(a, at: ["a"], derivedFrom: nil)
      let a7 = try t.add(a, at: ["a", "b"], derivedFrom: a6)
      let a8 = try t.add(a, at: ["a", "b"], derivedFrom: a7)
      accessPaths = [
        (a6, ["a"]),
        (a7, ["a", "b"]),
        (a8, ["a", "b"]),
      ]
      while !accessPaths.isEmpty {
        try checkCanNotUse(till: accessPaths.count - 1)
        let (a, p) = accessPaths.popLast()!
        try t.end(a, at: p)
      }
    }
  }

}
