/// A finite, directed graph.
public struct DirectedGraph<Vertex: Hashable, Label> {

  /// The base of an edge.
  public typealias EdgeBase = (source: Vertex, label: Label)

  /// The tip of an edge.
  public typealias EdgeTip = (target: Vertex, label: Label)

  /// A matrix representing the edges of the graph.
  fileprivate var edges: [Vertex: [Vertex: Label]]

  /// Creates an empty graph.
  public init() {
    edges = [:]
  }

  /// Inserts an edge from `source` to `target`, labeled by `label`.
  ///
  /// - Returns: `(true, label)` if there was no edge between `source` and `target`. Otherwise,
  ///   `(false, currentLabel)`, where `currentLabel` is label of the existing edge.
  ///
  /// - Complexity: O(1).
  @discardableResult
  public mutating func insertEdge(from source: Vertex, to target: Vertex, labeledBy label: Label)
    -> (inserted: Bool, labelAfterInsert: Label)
  {
    modifying(
      &edges[source, default: [:]],
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
    modifying(
      &edges[source, default: [:]],
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
    _read { yield edges[source]?[target] }
    _modify { yield &edges[source, default: [:]][target] }
  }

  /// Accesses the outgoing edges of `source`.
  ///
  /// - Complexity: O(1).
  public subscript(from source: Vertex) -> [Vertex: Label] {
    _read { yield edges[source, default: [:]] }
    _modify { yield &edges[source, default: [:]] }
  }

}

extension DirectedGraph where Label == () {

  /// Inserts an edge from `source` to `target`, labeled by `label`.
  ///
  /// - Returns: `true` if there was no edge between `source` and `target`. Otherwise, `false`.
  ///
  /// - Complexity: O(1).
  @discardableResult
  public mutating func insertEdge(from source: Vertex, to target: Vertex) -> Bool {
    insertEdge(from: source, to: target, labeledBy: ()).inserted
  }

}

extension DirectedGraph: Equatable where Label: Equatable {

  public static func == (l: Self, r: Self) -> Bool {
    var sources: Set<Vertex> = []
    sources.reserveCapacity(l.edges.count)

    for (source, lhs) in l.edges {
      let rhs = r.edges[source, default: [:]]
      if lhs.count != rhs.count { return false }
      for (target, label) in lhs {
        if rhs[target] != label { return false }
      }
      sources.insert(source)
    }

    return r.edges.keys.allSatisfy(sources.contains(_:))
  }

}

extension DirectedGraph: Hashable where Label: Hashable {

  public func hash(into hasher: inout Hasher) {
    var h = 0
    for (source, tips) in edges where !tips.isEmpty {
      var _hasher = hasher
      _hasher.combine(source)
      _hasher.combine(tips)
      h ^= _hasher.finalize()
    }
    hasher.combine(h)
  }

}
