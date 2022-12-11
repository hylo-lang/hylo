import Utils

/// A control-flow graph.
///
/// This data structure describes relation between the basic blocks of a function. The direction of
/// the graph's edges denotes the direction of the control flow from one block to another: there an
/// edge from `A` to `B` if the former's terminator points to the latter.
struct ControlFlowGraph {

  /// A vertex.
  typealias Vertex = Function.BlockAddress

  /// An control edge label.
  enum Label {

    /// A label denoting that the source is a predecessor of the target.
    case forward

    /// A label denoting that the source is a successor of the target.
    case backward

    /// A label denoting that the source is both a predecessor and successor of the target.
    case bidirectional

  }

  /// The relation encoded by the graph.
  fileprivate var relation: DirectedGraph<Vertex, Label>

  /// Creates an empty control flow graph.
  init() { relation = DirectedGraph() }

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
    default: break
    }
  }

  /// Returns the successors of `source`.
  func successors(of source: Vertex) -> [Vertex] {
    relation[from: source].compactMap({ tip in tip.value != .backward ? tip.key : nil })
  }

  /// Returns the predecessors of `target`.
  func predecessors(of target: Vertex) -> [Vertex] {
    relation[from: target].compactMap({ tip in tip.value != .forward ? tip.key : nil })
  }

}
