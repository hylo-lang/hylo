import Utils
import XCTest

final class DirectedGraphTests: XCTestCase {

  func testInsertEdge() {
    var g = DirectedGraph<Int, Int>()

    let x0 = g.insertEdge(from: 0, to: 0, labeledBy: 42)
    XCTAssert(x0.inserted)
    XCTAssertEqual(x0.labelAfterInsert, 42)

    let x1 = g.insertEdge(from: 0, to: 1, labeledBy: 42)
    XCTAssert(x1.inserted)
    XCTAssertEqual(x1.labelAfterInsert, 42)

    let x2 = g.insertEdge(from: 0, to: 0, labeledBy: 1337)
    XCTAssertFalse(x2.inserted)
    XCTAssertEqual(x2.labelAfterInsert, 42)
  }

  func testInsertEdgeWithoutLabel() {
    var g = DirectedGraph<Int, NoLabel>()

    XCTAssert(g.insertEdge(from: 0, to: 0))
    XCTAssert(g.insertEdge(from: 0, to: 1))
    XCTAssertFalse(g.insertEdge(from: 0, to: 0))
  }

  func testRemoveEdge() {
    var g = DirectedGraph<Int, Int>()

    g.insertEdge(from: 0, to: 0, labeledBy: 42)
    XCTAssertEqual(g.removeEdge(from: 0, to: 0), 42)
    XCTAssertNil(g.removeEdge(from: 0, to: 0))
  }

  func testAccessTarget() {
    var g = DirectedGraph<Int, Int>()

    g.insertEdge(from: 0, to: 0, labeledBy: 1)
    g.insertEdge(from: 0, to: 1, labeledBy: 2)
    XCTAssertEqual(g[from: 0, to: 0], 1)
    XCTAssertEqual(g[from: 0, to: 1], 2)
    XCTAssertNil(g[from: 0, to: 2])
    XCTAssertNil(g[from: 2, to: 0])

    g[from: 0, to: 2] = 3
    g[from: 2, to: 0] = 3
    XCTAssertEqual(g[from: 0, to: 2], 3)
    XCTAssertEqual(g[from: 2, to: 0], 3)
  }

  func testAccessOutgoingEdges() {
    var g = DirectedGraph<Int, Int>()

    g.insertEdge(from: 0, to: 0, labeledBy: 1)
    g.insertEdge(from: 0, to: 1, labeledBy: 2)
    XCTAssertEqual(g[from: 0], [0: 1, 1: 2])
    XCTAssertEqual(g[from: 2], [:])

    g[from: 0] = [2: 3]
    g[from: 2] = [0: 3]
    XCTAssertEqual(g[from: 0], [2: 3])
    XCTAssertEqual(g[from: 2], [0: 3])
  }

  func testEdges() {
    var g = DirectedGraph<Int, String>()

    let edges = [(0, "a", 1), (0, "b", 2), (1, "c", 3)]
    for e in edges {
      g[from: e.0, to: e.2] = e.1
    }
    XCTAssert(g.edges.sorted().elementsEqual(edges, by: { $0 == $1 }))
  }

  func testBFS() {
    var g = DirectedGraph<Int, NoLabel>()
    g.insertEdge(from: 0, to: 1)
    g.insertEdge(from: 0, to: 2)
    g.insertEdge(from: 1, to: 3)
    g.insertEdge(from: 2, to: 3)

    let vertices = Array(g.bfs(from: 0))
    XCTAssertEqual(Set(vertices), [0, 1, 2, 3])
    XCTAssertEqual(vertices.first, 0)
    XCTAssertEqual(vertices.last, 3)
  }

  func testIsReachable() {
    var g = DirectedGraph<Int, NoLabel>()
    g.insertEdge(from: 0, to: 1)
    g.insertEdge(from: 0, to: 2)
    g.insertEdge(from: 1, to: 3)
    g.insertEdge(from: 2, to: 3)

    XCTAssert(g.isReachable(3, from: 0))
    XCTAssert(g.isReachable(2, from: 0))
    XCTAssert(g.isReachable(3, from: 1))

    XCTAssertFalse(g.isReachable(0, from: 3))
    XCTAssertFalse(g.isReachable(2, from: 1))
  }

}

extension DirectedGraph<Int, String>.Edge {

  fileprivate static func == (_ l: Self, r: (Int, String, Int)) -> Bool {
    (l.source == r.0) && (l.label == r.1) && (l.target == r.2)
  }

}
