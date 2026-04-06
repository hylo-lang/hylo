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

}
