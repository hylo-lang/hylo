import FrontEnd
import TestUtils
import Utils
import XCTest

@testable import Interpreter

extension String: Regular {}

final class AccessTrackerTests: XCTestCase {

  typealias Error = AccessTracker<String>.Error

  func testBeginAccessWhenSubPartProjectionDoesNotExists() throws {
    for p in [.let, .inout, .sink, .set] as [AccessEffect] {
      var t = AccessTracker("a", with: .sink)
      _ = try t.begin(p, at: ["b"])
      for p in [.let, .inout, .sink, .set] as [AccessEffect] {
        _ = try t.begin(p, at: ["b", "x"])
        _ = try t.begin(p, at: ["b", "x"])
      }
    }
  }

  func testInvalidBeginAccessOnRootNodeWhenSubPartProjectionExists() throws {
    for p in [.inout, .sink, .set] as [AccessEffect] {
      var t = AccessTracker("a", with: p)
      _ = try t.begin(.sink, at: ["b"])
      for a in [.let, .inout, .sink, .set] as [AccessEffect] {
        check(throws: Error.overlappingExclusiveAccessExists(for: "a")) {
          _ = try t.begin(a, at: [])
        }
      }
    }
  }

  func testInvalidBeginAccessOnNonRootNodeWhenSubPartProjectionExists() throws {
    for p in [.inout, .sink, .set] as [AccessEffect] {
      var t = AccessTracker("a", with: p)
      _ = try t.begin(.sink, at: ["b"])
      _ = try t.begin(.sink, at: ["b", "p"])
      for a in [.let, .inout, .sink, .set] as [AccessEffect] {
        check(throws: Error.overlappingExclusiveAccessExists(for: "b")) {
          _ = try t.begin(a, at: ["b"])
        }
      }
    }
  }

  func testBeginAccessForLetAccessWhenSubPartProjectionExists() throws {
    var t = AccessTracker("a", with: .let)
    _ = try t.begin(.let, at: ["b"])
    _ = try t.begin(.let, at: [])
    _ = try t.begin(.let, at: ["b", "p"])
    _ = try t.begin(.let, at: ["b"])
    _ = try t.begin(.let, at: [])
  }

  func testRequireIsActive() throws {
    for p in [.inout, .sink, .set] as [AccessEffect] {
      var t = AccessTracker("a", with: p)
      let a1 = try t.begin(p, at: ["b"])
      try t.requireIsActive(a1, in: ["b"])
      let a2 = try t.begin(p, at: ["b", "c"])
      check(throws: Error.overlappingExclusiveAccessExists(for: "b")) {
        try t.requireIsActive(a1, in: ["b"])
      }
      try t.requireIsActive(a2, in: ["b", "c"])
      let a3 = try t.begin(p, at: ["b", "c"])
      check(throws: Error.overlappingExclusiveAccessExists(for: "b")) {
        try t.requireIsActive(a1, in: ["b"])
      }
      check(throws: Error.overlappingExclusiveAccessExists(for: "c")) {
        try t.requireIsActive(a2, in: ["b", "c"])
      }
      try t.requireIsActive(a3, in: ["b", "c"])
      // sibling should be active.
      let a4 = try t.begin(p, at: ["b", "d"])
      try t.requireIsActive(a4, in: ["b", "d"])
      try t.requireIsActive(a3, in: ["b", "c"])
    }

    var t = AccessTracker("a", with: .let)
    let a1 = try t.begin(.let, at: ["b"])
    try t.requireIsActive(a1, in: ["b"])
    let a2 = try t.begin(.let, at: ["b", "c"])
    try t.requireIsActive(a1, in: ["b"])
    try t.requireIsActive(a2, in: ["b", "c"])
    let a3 = try t.begin(.let, at: ["b", "c"])
    try t.requireIsActive(a1, in: ["b"])
    try t.requireIsActive(a2, in: ["b", "c"])
    try t.requireIsActive(a3, in: ["b", "c"])
    // sibling should be active.
    let a4 = try t.begin(.let, at: ["b", "d"])
    try t.requireIsActive(a4, in: ["b", "d"])
    try t.requireIsActive(a3, in: ["b", "c"])
  }

  func testEndAccess() throws {
    for p in [.inout, .sink, .set] as [AccessEffect] {
      var t = AccessTracker("a", with: p)
      let a1 = try t.begin(p, at: ["b", "c"])
      let a2 = try t.begin(p, at: ["b", "c", "d"])
      let a3 = try t.begin(p, at: ["b", "c", "d"])
      check(throws: Error.overlappingExclusiveAccessExists(for: "d")) {
        try t.end(a2, at: ["b", "c", "d"])
      }
      check(throws: Error.overlappingExclusiveAccessExists(for: "c")) {
        try t.end(a1, at: ["b", "c"])
      }
      try t.end(a3, at: ["b", "c", "d"])
      try t.end(a2, at: ["b", "c", "d"])

      let a4 = try t.begin(p, at: ["b", "c"])  // can start new access after no overlapping subpart
      try t.end(a4, at: ["b", "c"])
      try t.end(a1, at: ["b", "c"])
      let a5 = try t.begin(p, at: [])  // can start new access after no overlapping subpart
      try t.end(a5, at: [])
    }

    // Let access allows ending access in arbitrary order as all let access are always active.
    var t = AccessTracker("a", with: .let)
    let a1 = try t.begin(.let, at: ["b", "c"])
    let a2 = try t.begin(.let, at: ["b", "c", "d"])
    let a3 = try t.begin(.let, at: ["b", "c", "d"])
    try t.end(a1, at: ["b", "c"])
    try t.end(a2, at: ["b", "c", "d"])
    try t.end(a3, at: ["b", "c", "d"])
    let a4 = try t.begin(.inout, at: ["b", "c"])  // can start new access after no overlapping subpart
    try t.end(a4, at: ["b", "c"])
    let a5 = try t.begin(.inout, at: [])  // can start new access after no overlapping subpart
    try t.end(a5, at: [])
  }

  func testCanObserveAccesses() throws {
    var t = AccessTracker("a", with: .inout)
    _ = try t.begin(.inout, at: [])
    _ = try t.begin(.inout, at: ["b"])
    _ = try t.begin(.let, at: ["b"])
    _ = try t.begin(.inout, at: ["c", "d"])
    _ = try t.begin(.inout, at: ["c", "d"])

    XCTAssertEqual(t.accesses(along: []).map { $0.map { $0.effect } }, [[.inout, .inout]])
    XCTAssertEqual(
      t.accesses(along: ["b"]).map { $0.map { $0.effect } },
      [[.inout, .inout], [.inout, .let]])
    XCTAssertEqual(
      t.accesses(along: ["c", "d"]).map { $0.map { $0.effect } },
      [[.inout, .inout], [], [.inout, .inout]])
  }

}
