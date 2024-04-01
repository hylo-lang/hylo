import Algorithms
import XCTest

@testable import FrontEnd

final class NodeKindTests: XCTestCase {

  func testEqual() {
    for t in NodeKind.allValues {
      XCTAssert(NodeKind(t) == NodeKind(t))
      XCTAssert(NodeKind(t) == t)
      XCTAssert(t == NodeKind(t))
    }

    var a = NodeKind.allValues
    a.rotate(toStartAt: 1)
    for (t, u) in zip(NodeKind.allValues, a) {
      XCTAssert(!(NodeKind(t) == NodeKind(u)))
      XCTAssert(!(NodeKind(t) == u))
      XCTAssert(!(t == NodeKind(u)))
    }
  }

  func testNotEqual() {
    for t in NodeKind.allValues {
      XCTAssert(!(NodeKind(t) != NodeKind(t)))
      XCTAssert(!(NodeKind(t) != t))
      XCTAssert(!(t != NodeKind(t)))
    }

    var a = NodeKind.allValues
    a.rotate(toStartAt: 1)
    for (t, u) in zip(NodeKind.allValues, a) {
      XCTAssert(NodeKind(t) != NodeKind(u))
      XCTAssert(NodeKind(t) != u)
      XCTAssert(t != NodeKind(u))
    }
  }

  func testHash() {
    for t in NodeKind.allValues {
      XCTAssertEqual(NodeKind(t).hashValue, NodeKind(t).hashValue)
    }
  }

}
