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

  /// The type of a table mapping a vertex to its outgoing edges.
  private typealias Out = [Vertex: OutgoingEdges]

  /// A table from a vertex to its outgoing edges.
  private var out: Out

  /// Creates an empty graph.
  public init() {
    out = [:]
  }

  /// The vertices of the graph.
  public var vertices: some Collection<Vertex> {
    out.keys
  }

  /// The edges of the graph.
  public var edges: some Collection<Edge> {
    out.lazy
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
    _ = out[target].setIfNil([:])
    return modify(&out[source, default: [:]]) { (tips) in
      if let currentLabel = tips[target] {
        return (false, currentLabel)
      } else {
        tips[target] = label
        return (true, label)
      }
    }
  }

  /// Removes the edge from `source` to `target`.
  ///
  /// - Returns: The label of the removed edge, or `nil` if there was no edge to remove.
  ///
  /// - Complexity: O(1).
  @discardableResult
  public mutating func removeEdge(from source: Vertex, to target: Vertex) -> Label? {
    modify(&out[source, default: [:]]) { (tips) in
      if let i = tips.index(forKey: target) {
        defer { tips.remove(at: i) }
        return tips[i].value
      } else {
        return nil
      }
    }
  }

  /// Accesses the label on the edge from `source` to `target`.
  ///
  /// - Returns: If there exists a edge from from `source` to `target`, the label of that edge.
  ///   Otherwise, `nil`.
  public subscript(from source: Vertex, to target: Vertex) -> Label? {
    _read { yield out[source]?[target] }
    _modify { yield &out[source, default: [:]][target] }
  }

  /// Accesses the outgoing edges of `source`.
  ///
  /// - Complexity: O(1).
  public subscript(from source: Vertex) -> OutgoingEdges {
    _read { yield out[source, default: [:]] }
    _modify { yield &out[source, default: [:]] }
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
    sources.reserveCapacity(l.out.count)

    for (source, lhs) in l.out {
      let rhs = r.out[source, default: [:]]
      if lhs.count != rhs.count { return false }
      for (target, label) in lhs {
        if rhs[target] != label { return false }
      }
      sources.insert(source)
    }

    return r.out.keys.allSatisfy(sources.contains(_:))
  }

}

extension DirectedGraph: Hashable where Label: Hashable {

  public func hash(into hasher: inout Hasher) {
    var h = 0
    for (source, tips) in out where !tips.isEmpty {
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

/// The strongly connected components of a directed graph.
public struct StronglyConnectedComponents<Vertex: Hashable, Label> {

  /// The graph whose strongly connected components are discovered.
  public let graph: DirectedGraph<Vertex, Label>

  /// A map from vertex identifier to its properties.
  private var properties: [Vertex: Properties] = [:]

  /// A map from component to its vertices.
  private var components: [[Vertex]] = []

  /// The stack of vertices being visited.
  private var stack: [Vertex] = []

  /// The next vertex index.
  private var nextIndex: Int = 0

  /// Creates an instance containing the strongly connected components of `graph`.
  public init(_ graph: DirectedGraph<Vertex, Label>) {
    self.graph = graph
  }

  /// Returns the vertices in the given component.
  public func vertices(in c: Int) -> [Vertex] {
    components[c]
  }

  /// Returns all strongly connected components.
  public mutating func all() -> [[Vertex]] {
    for v in graph.vertices {
      _ = component(containing: v)
    }
    return components
  }

  /// Returns the strongly connected component containing `v`.
  public mutating func component(containing v: Vertex) -> Int {
    if let c = properties[v]?.component { return c }

    properties[v] = .init(low: nextIndex, index: nextIndex, component: -1, isOnStack: true)
    stack.append(v)
    nextIndex += 1

    for (w, _) in graph[from: v] {
      if properties[w] == nil {
        // Successor w has not yet been visited; recurse on it
        _  = component(containing: w)
        properties[v]!.low = min(properties[v]!.low, properties[w]!.low)
      } else if properties[w]!.isOnStack {
        properties[v]!.low = min(properties[v]!.low, properties[w]!.index)
      }
    }

    if properties[v]!.low == properties[v]!.index {
      let c = components.count
      components.append([])

      while true {
        let w = stack.removeLast()
        components[c].append(w)
        modify(&properties[w]!) { (pw) in
          pw.isOnStack = false
          pw.component = c
        }
        if (w == v) { break }
      }
    }

    return properties[v]!.component
  }

  /// The properties associated with a vertex in Tarjan's algorithm.
  private struct Properties {

    /// The smallest index of any vertex reachable from this vertex.
    var low: Int = -1

    /// The index of this vertex.
    var index: Int = -1

    /// The component in which this vertex belongs.
    var component: Int = -1

    /// True iff this vertex is on the stack.
    var isOnStack: Bool = false

  }

}
