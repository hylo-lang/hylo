/// A collection of unordered lists representing a directed graph.
public struct AdjacencyList<Vertex, Label> where Vertex: Hashable {

  public typealias EdgeBase = (source: Vertex, label: Label)

  public typealias EdgeTip = (target: Vertex, label: Label)

  fileprivate var links: [Vertex: [EdgeTip]]

  /// Creates an empty graph.
  public init() {
    links = [:]
  }

  /// Inserts an edge into the graph.
  ///
  /// - Parameters:
  ///   - source: The source of the new edge.
  ///   - target: The target of the new edge.
  ///   - label: The label of the new edge.
  /// - Returns: `(true, label)` if there was no edge between `source` and `target`. Otherwise, the
  ///   method returns `(false, currentLabel)`, where `currentLabel` is label of the existing edge.
  @discardableResult
  public mutating func insertEdge(
    from source: Vertex, to target: Vertex, labeledBy label: Label
  ) -> (inserted: Bool, labelAfterInsert: Label) {
    if let tip = links[source, default: []].first(where: { $0.target == target }) {
      return (inserted: false, labelAfterInsert: tip.label)
    } else {
      links[source, default: []].append((target: target, label: label))
      return (inserted: true, labelAfterInsert: label)
    }
  }

  /// Removes an edge from the graph.
  ///
  /// - Parameters:
  ///   - source: The source of the edge to remove.
  ///   - target: The target of the edge to remove.
  /// - Returns: If there was an edge from from `source` to `target`, the label of that edge;
  ///   otherwise, `nil`.
  @discardableResult
  public mutating func removeEdge(from source: Vertex, to target: Vertex) -> Label? {
    if let i = links[source, default: []].firstIndex(where: { $0.target == target }) {
      let label = links[source]![i].label
      links[source]!.remove(at: i)
      if links[source]!.isEmpty { links[source] = nil }
      return label
    } else {
      return nil
    }
  }

  /// Accesses the label on the edge from `source` to `target` for reading and writing.
  ///
  /// - Parameters:
  ///   - source: The source of the new edge.
  ///   - target: The target of the new edge.
  /// - Returns: If there exists a edge from from `source` to `target`, the label of that edge;
  ///   otherwise, `nil`.
  public subscript(source: Vertex, target: Vertex) -> Label? {
    get {
      if let tip = links[source, default: []].first(where: { $0.target == target }) {
        return tip.label
      } else {
        return nil
      }
    }
    set {
      if let i = links[source, default: []].firstIndex(where: { $0.target == target }) {
        if let label = newValue {
          links[source]![i].label = label
        } else {
          links[source]!.remove(at: i)
          if links[source]!.isEmpty { links[source] = nil }
        }
      } else {
        if let label = newValue {
          links[source, default: []].append((target: target, label: label))
        }
      }
    }
  }

  /// Returns the tip of the edges that start from `source`.
  ///
  /// - Parameter source: A source vertex.
  public func edges(from source: Vertex) -> [EdgeTip] {
    return links[source] ?? []
  }

  /// Returns the tip of the edges that end at `target`.
  ///
  /// - Complexity: O(n^2) where n is the number of vertices in the graph.
  ///
  /// - Parameter target: A target vertex.
  public func edges(to target: Vertex) -> [EdgeBase] {
    var bases: [EdgeBase] = []
    for (source, tips) in links {
      bases.append(contentsOf: tips.compactMap({ (tip) -> EdgeBase? in
        return tip.target == target
          ? (source: source, label: tip.label)
          : nil
      }))
    }
    return bases
  }

}

extension AdjacencyList: Collection {

  public typealias Element = (source: Vertex, target: Vertex, label: Label)

  public struct Index: Comparable {

    fileprivate var linkIndex: Dictionary<Vertex, [EdgeTip]>.Index

    fileprivate var edgeIndex: Int

    public static func < (lhs: Index, rhs: Index) -> Bool {
      return lhs.linkIndex == rhs.linkIndex
        ? lhs.edgeIndex < rhs.edgeIndex
        : lhs.linkIndex < rhs.linkIndex
    }

  }

  public var startIndex: Index {
    return Index(linkIndex: links.startIndex, edgeIndex: 0)
  }

  public var endIndex: Index {
    return Index(linkIndex: links.endIndex, edgeIndex: 0)
  }

  public func index(after i: Index) -> Index {
    if i.edgeIndex < links[i.linkIndex].value.endIndex {
      return Index(linkIndex: i.linkIndex, edgeIndex: i.edgeIndex + 1)
    } else {
      return Index(linkIndex: links.index(after: i.linkIndex), edgeIndex: 0)
    }
  }

  public subscript(position: Index) -> Element {
    let link = links[position.linkIndex]
    let tip = link.value[position.edgeIndex]
    return (link.key, tip.target, tip.label)
  }

}

extension AdjacencyList where Label == Void {

  /// Inserts an edge into the graph.
  ///
  /// - Parameters:
  ///   - source: The source of the new edge.
  ///   - target: The target of the new edge.
  /// - Returns: `true` if there was no edge between `source` and `target`; otherwise, `false`.
  @discardableResult
  public mutating func insertEdge(from source: Vertex, to target: Vertex) -> Bool {
    if links[source, default: []].contains(where: { $0.target == target }) {
      return false
    } else {
      links[source, default: []].append((target: target, label: ()))
      return true
    }
  }

}
