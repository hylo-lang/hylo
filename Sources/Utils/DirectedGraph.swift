import DequeModule

/// The absence of a label in a directed graph.
public struct NoLabel: Hashable {

  /// Creates an instance.
  public init() {}

}

/// A finite, directed graph.
///
/// - Note: Use `DirectedGraph<V, NoLabel>` rather than `DirectedGraph<V, Void>` to implement an
///   unlabeled graph. Unlike `Void`, `NoLabel` conforms to `Equatable`, allowing the graph itself
///   to be `Equatable`.
public struct DirectedGraph<Vertex: Hashable, Label> {

  /// The base of an edge.
  public typealias EdgeBase = (source: Vertex, label: Label)

  /// The tip of an edge.
  public typealias EdgeTip = (target: Vertex, label: Label)

  /// A collection with the outgoing edges of a vertex.
  public typealias OutgoingEdges = [Vertex: Label]

  /// An edge between two vertices.
  public struct Edge {

    /// The vertex at the base of the edge.
    public let source: Vertex

    /// The label of the vertex.
    public let label: Label

    /// The vertex at the tip of the edge.
    public let target: Vertex

  }

  /// A table from a vertex to its outgoing edges.
  private var outgoingEdges: [Vertex: OutgoingEdges]

  /// Creates an empty graph.
  public init() {
    outgoingEdges = [:]
  }

  /// The edges of the graph.
  public var edges: some Collection<Edge> {
    outgoingEdges.lazy
      .map({ (s, o) in o.lazy.map({ (t, l) in .init(source: s, label: l, target: t) }) })
      .joined()
  }

  /// Returns the vertices in `self` gathered in a breadth-first manner from `root`.
  public func bfs(from root: Vertex) -> BreadthFirstSequence<Vertex, Label> {
    .init(from: root, in: self)
  }

  /// Returns `true` iff there exists a path from `u` to `v` in the graph.
  ///
  /// - Complexity: O(m) where m is the number of edges in the graph.
  public func isReachable(_ v: Vertex, from u: Vertex) -> Bool {
    bfs(from: u).contains(v)
  }

  /// Inserts an edge from `source` to `target`, labeled by `label`.
  ///
  /// - Returns: `(true, label)` if there was no edge between `source` and `target`. Otherwise,
  ///   `(false, currentLabel)`, where `currentLabel` is label of the existing edge.
  ///
  /// - Complexity: O(1).
  @discardableResult
  public mutating func insertEdge(
    from source: Vertex, to target: Vertex, labeledBy label: Label
  ) -> (inserted: Bool, labelAfterInsert: Label) {
    modify(
      &outgoingEdges[source, default: [:]],
      { tips in
        if let currentLabel = tips[target] {
          return (false, currentLabel)
        } else {
          tips[target] = label
          return (true, label)
        }
      })
  }

  /// Removes the edge from `source` to `target`.
  ///
  /// - Returns: The label of the removed edge, or `nil` if there was no edge to remove.
  ///
  /// - Complexity: O(1).
  @discardableResult
  public mutating func removeEdge(from source: Vertex, to target: Vertex) -> Label? {
    modify(
      &outgoingEdges[source, default: [:]],
      { tips in
        if let i = tips.index(forKey: target) {
          defer { tips.remove(at: i) }
          return tips[i].value
        } else {
          return nil
        }
      })
  }

  /// Accesses the label on the edge from `source` to `target`.
  ///
  /// - Returns: If there exists a edge from from `source` to `target`, the label of that edge.
  ///   Otherwise, `nil`.
  public subscript(from source: Vertex, to target: Vertex) -> Label? {
    _read { yield outgoingEdges[source]?[target] }
    _modify { yield &outgoingEdges[source, default: [:]][target] }
  }

  /// Accesses the outgoing edges of `source`.
  ///
  /// - Complexity: O(1).
  public subscript(from source: Vertex) -> OutgoingEdges {
    _read { yield outgoingEdges[source, default: [:]] }
    _modify { yield &outgoingEdges[source, default: [:]] }
  }

}

extension DirectedGraph where Label == NoLabel {

  /// Inserts an edge from `source` to `target`, labeled by `label`.
  ///
  /// - Returns: `true` if there was no edge between `source` and `target`. Otherwise, `false`.
  ///
  /// - Complexity: O(1).
  @discardableResult
  public mutating func insertEdge(from source: Vertex, to target: Vertex) -> Bool {
    insertEdge(from: source, to: target, labeledBy: .init()).inserted
  }

}

extension DirectedGraph: Equatable where Label: Equatable {

  public static func == (l: Self, r: Self) -> Bool {
    var sources: Set<Vertex> = []
    sources.reserveCapacity(l.outgoingEdges.count)

    for (source, lhs) in l.outgoingEdges {
      let rhs = r.outgoingEdges[source, default: [:]]
      if lhs.count != rhs.count { return false }
      for (target, label) in lhs {
        if rhs[target] != label { return false }
      }
      sources.insert(source)
    }

    return r.outgoingEdges.keys.allSatisfy(sources.contains(_:))
  }

}

extension DirectedGraph: Hashable where Label: Hashable {

  public func hash(into hasher: inout Hasher) {
    var h = 0
    for (source, tips) in outgoingEdges where !tips.isEmpty {
      var _hasher = hasher
      _hasher.combine(source)
      _hasher.combine(tips)
      h ^= _hasher.finalize()
    }
    hasher.combine(h)
  }

}

extension DirectedGraph.Edge: Equatable where Label: Equatable {}

extension DirectedGraph.Edge: Comparable where Vertex: Comparable, Label: Comparable {

  public static func < (l: Self, r: Self) -> Bool {
    if l.source == r.source {
      return (l.label == r.label) ? (l.target < r.target) : (l.label < r.label)
    } else {
      return l.source < r.source
    }
  }

}

/// The vertices of a graph collected in a breadth-first manner.
public struct BreadthFirstSequence<Vertex: Hashable, Label>: IteratorProtocol, Sequence {

  /// The graph whose the vertices are collected.
  public let graph: DirectedGraph<Vertex, Label>

  /// A queue with vertices to visit.
  private var toVisit = Deque<Vertex>()

  /// The vertices that have been returned already.
  private var visited = Set<Vertex>()

  /// Creates an instance containing the vertices in `graph` gathered in a breadth-first manner
  /// from `root`.
  public init(from root: Vertex, in graph: DirectedGraph<Vertex, Label>) {
    self.graph = graph
    self.toVisit.append(root)
  }

  /// Returns the next vertex, or `nil` if all vertices have been returned already.
  public mutating func next() -> Vertex? {
    guard let v = toVisit.popFirst() else { return nil }
    visited.insert(v)
    for e in graph[from: v] where !visited.contains(e.key) {
      toVisit.append(e.key)
    }
    return v
  }

}
