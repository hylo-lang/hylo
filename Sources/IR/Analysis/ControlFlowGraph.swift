import Utils

/// A control-flow graph.
///
/// This data structure describes relation between the basic blocks of a function. The direction of
/// the graph's edges denotes the direction of the control flow from one block to another: there an
/// edge from `A` to `B` if the former's terminator points to the latter.
struct ControlFlowGraph: Sendable {

  /// A node in the graph.
  typealias Vertex = Function.Blocks.Address

  /// An control edge label.
  enum Label: Sendable {

    /// A label denoting that the source is a predecessor of the target.
    case forward

    /// A label denoting that the source is a successor of the target.
    case backward

    /// A label denoting that the source is both a predecessor and successor of the target.
    case bidirectional

  }

  /// The way a control-flow relation is represented internally.
  private typealias Relation = DirectedGraph<Vertex, Label>

  /// The relation encoded by the graph.
  private var relation: Relation

  /// Creates an empty control flow graph.
  init() {
    relation = DirectedGraph()
  }

  /// Defines `source` as a predecessor of `target`.
  mutating func define(_ source: Vertex, predecessorOf target: Vertex) {
    let (inserted, label) = relation.insertEdge(from: source, to: target, labeledBy: .forward)
    if inserted {
      relation[from: target, to: source] = .backward
    } else if label == .backward {
      relation[from: source, to: target] = .bidirectional
      relation[from: target, to: source] = .bidirectional
    }
  }

  /// Removes `source` from the predecessors of `target`.
  mutating func remove(_ source: Vertex, fromPredecessorsOf target: Vertex) {
    switch relation[from: source, to: target] {
    case .forward:
      relation[from: source, to: target] = nil
      relation[from: target, to: source] = nil
    case .bidirectional:
      relation[from: source, to: target] = .backward
      relation[from: target, to: source] = .forward
    default:
      break
    }
  }

  /// Returns the successors of `source`.
  func successors(of source: Vertex) -> [Vertex] {
    relation[from: source].compactMap({ tip in
      tip.value != .backward ? tip.key : nil
    })
  }

  /// Returns the predecessors of `target`.
  func predecessors(of target: Vertex) -> [Vertex] {
    relation[from: target].compactMap({ tip in
      tip.value != .forward ? tip.key : nil
    })
  }

  /// A collection where the vertex at index `i + 1` is predecessor of the vertex at index `i`.
  typealias PredecessorPath = [Vertex]

  /// Returns the paths originating at `ancestor` and reaching `destination` excluding those that
  /// contain `destination`.
  func paths(to destination: Vertex, from ancestor: Vertex) -> [PredecessorPath] {
    var v: Set = [destination]
    var c: [Vertex: [PredecessorPath]] = [:]
    return paths(to: destination, from: ancestor, notContaining: &v, cachingResultsTo: &c)
  }

  /// Returns the paths originating at `ancestor` and reaching `destination` excluding those that
  /// contain the vertices in `visited`.
  private func paths(
    to destination: Vertex, from ancestor: Vertex, notContaining visited: inout Set<Vertex>,
    cachingResultsTo cache: inout [Vertex: [PredecessorPath]]
  ) -> [PredecessorPath] {
    if destination == ancestor { return [[]] }
    if let r = cache[destination] { return r }

    var result: [PredecessorPath] = []
    visited.insert(destination)
    for p in predecessors(of: destination) where !visited.contains(p) {
      let s = paths(
        to: p, from: ancestor, notContaining: &visited, cachingResultsTo: &cache)
      result.append(contentsOf: s.map({ [p] + $0 }))
    }
    visited.remove(destination)

    cache[destination] = result
    return result
  }

}

extension ControlFlowGraph: CustomStringConvertible {

  /// The Graphviz (dot) representation of the graph.
  var description: String {
    var result = "strict digraph CFG {\n\n"
    for e in relation.edges {
      switch e.label {
      case .forward:
        result.write("\(e.source) -> \(e.target);\n")
      case .bidirectional:
        result.write("\(e.source) -> \(e.target);\n")
        result.write("\(e.target) -> \(e.source);\n")
      case .backward:
        continue
      }
    }
    result.write("\n}")
    return result
  }

}
