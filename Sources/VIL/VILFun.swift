import AST
import Basic

/// A control flow grahp.
///
/// A control flow graph describes the relation between the basic blocks of the function. The
/// direction of of its edges denotes the direction of the control flow from one block to another:
/// there an edge from `A` to `B` if the former's terminator points to the latter.
///
/// The graph is represented as an adjacency list that encodes the successor and the predecessor
/// relations, enabling efficient lookup in both directions.
public typealias ControlFlowGraph = AdjacencyList<BasicBlockIndex, ControlEdge>

/// An edge in a control flow graph.
public enum ControlEdge {

  /// An edge `A -> B` denoting that `A` is a predecessor of `B`.
  case forward

  /// An edge `A -> B` denoting that `A` is a successor of `B`.
  case backward

  /// An edge `A -> B` denoting that `A` is a predecessor *and* a successor of `B`.
  case bidirectional

}

/// A VIL function.
public struct VILFun {

  /// The mangled name of the function.
  public let name: String

  /// An optional debug name describing the function.
  public let debugName: String?

  /// The VIL type of the function.
  public let type: VILType

  /// The indices of the basic blocks in the function, starting with the entry.
  public var blocks: [BasicBlockIndex] = []

  /// The control flow graph of the function.
  public var cfg = ControlFlowGraph()

  /// Creates a new VIL function.
  ///
  /// - Parameters:
  ///   - name: The name of the function.
  ///   - debugName: An optional debug name describing the function.
  ///   - type: The (unapplied) type of the function.
  init(name: String, debugName: String? = nil, type: VILType) {
    self.name = name
    self.debugName = debugName
    self.type = type
  }

  /// A Boolean value that indicates whether the function has an entry block.
  public var hasEntry: Bool { !blocks.isEmpty }

  /// The index of the function's entry block.
  public var entry: BasicBlockIndex? { blocks.first }

  /// Returns the basic block in this function that contains the specified instruction.
  ///
  /// - Parameters:
  ///   - inst: An instruction index.
  ///   - module: The module in which `inst` is defined.
  public func block(containing inst: InstIndex, in module: Module) -> BasicBlockIndex? {
    return blocks.first(where: { block in
      module.blocks[block].instructions.contains(inst)
    })
  }

  /// Inserts a control edge from one basic block to another.
  mutating func insertControlEdge(from source: BasicBlockIndex, to target: BasicBlockIndex) {
    assert(blocks.contains(source) && blocks.contains(target))

    let (inserted, label) = cfg.insertEdge(from: source, to: target, labeledBy: .forward)
    if inserted {
      cfg[target, source] = .backward
    } else if label == .backward {
      cfg[source, target] = .bidirectional
      cfg[target, source] = .bidirectional
    }
  }

}
