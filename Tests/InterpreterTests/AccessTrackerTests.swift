import Interpreter
import TestUtils
import Utils
import XCTest

extension String: Regular {}

final class AccessTrackerTests: XCTestCase {

  typealias Error = AccessTracker<String>.Error

  func testBeginAccessWhenSubPartProjectionNotExists() throws {
    for p in [.let, .inout, .sink, .set] as [AccessKind] {
      var t = AccessTracker("a", with: .sink)
      _ = try t.begin(p, at: ["b"])
      for p in [.let, .inout, .sink, .set] as [AccessKind] {
        _ = try t.begin(p, at: ["b", "x"])
        _ = try t.begin(p, at: ["b", "x"])
      }
    }
  }

  func testBeginAccessWhenSubPartProjectionExists() throws {
    // Add invalid access on root
    for p in [.inout, .sink, .set] as [AccessKind] {
      var t = AccessTracker("a", with: p)
      _ = try t.begin(.sink, at: ["b"])
      for a in [.let, .inout, .sink, .set] as [AccessKind] {
        check(throws: Error.overlappingExclusiveAccessExists(for: "a")) {
          _ = try t.begin(a, at: [])
        }
      }
    }

    // Add invalid access on non-root
    for p in [.inout, .sink, .set] as [AccessKind] {
      var t = AccessTracker("a", with: p)
      _ = try t.begin(.sink, at: ["b"])
      _ = try t.begin(.sink, at: ["b", "p"])
      for a in [.let, .inout, .sink, .set] as [AccessKind] {
        check(throws: Error.overlappingExclusiveAccessExists(for: "b")) {
          _ = try t.begin(a, at: ["b"])
        }
      }
    }

    // Let supports access to parent when all subpart projection are let.
    var t = AccessTracker("a", with: .let)
    _ = try t.begin(.let, at: ["b"])
    _ = try t.begin(.let, at: [])
    _ = try t.begin(.let, at: ["b", "p"])
    _ = try t.begin(.let, at: ["b"])
  }

  func testRequireIsActive() throws {
    for p in [.inout, .sink, .set] as [AccessKind] {
      var t = AccessTracker("a", with: p)
      let a1 = try t.begin(p, at: ["b"])
      try t.requireIsActive(a1, at: ["b"])
      let a2 = try t.begin(p, at: ["b", "c"])
      check(throws: Error.overlappingExclusiveAccessExists(for: "b")) {
        try t.requireIsActive(a1, at: ["b"])
      }
      try t.requireIsActive(a2, at: ["b", "c"])
      let a3 = try t.begin(p, at: ["b", "c"])
      check(throws: Error.overlappingExclusiveAccessExists(for: "b")) {
        try t.requireIsActive(a1, at: ["b"])
      }
      check(throws: Error.overlappingExclusiveAccessExists(for: "c")) {
        try t.requireIsActive(a2, at: ["b", "c"])
      }
      try t.requireIsActive(a3, at: ["b", "c"])
      // sibling should be active.
      let a4 = try t.begin(p, at: ["b", "d"])
      try t.requireIsActive(a4, at: ["b", "d"])
      try t.requireIsActive(a3, at: ["b", "c"])
    }

    var t = AccessTracker("a", with: .let)
    let a1 = try t.begin(.let, at: ["b"])
    try t.requireIsActive(a1, at: ["b"])
    let a2 = try t.begin(.let, at: ["b", "c"])
    try t.requireIsActive(a1, at: ["b"])
    try t.requireIsActive(a2, at: ["b", "c"])
    let a3 = try t.begin(.let, at: ["b", "c"])
    try t.requireIsActive(a1, at: ["b"])
    try t.requireIsActive(a2, at: ["b", "c"])
    try t.requireIsActive(a3, at: ["b", "c"])
    // sibling should be active.
    let a4 = try t.begin(.let, at: ["b", "d"])
    try t.requireIsActive(a4, at: ["b", "d"])
    try t.requireIsActive(a3, at: ["b", "c"])
  }

  func testEndAccess() throws {
    for p in [.inout, .sink, .set] as [AccessKind] {
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

}
