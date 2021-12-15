import AST
import Basic

/// A control-flow graph.
///
/// A control-flow graph describes the relation between the basic blocks of the function. The
/// direction of of its edges denotes the direction of the control flow from one block to another:
/// there an edge from `A` to `B` if the former's terminator points to the latter.
///
/// The graph is represented as an adjacency list that encodes all successor and the predecessor
/// relations, enabling efficient lookup in both directions.
public typealias ControlFlowGraph = AdjacencyList<BasicBlockIndex, ControlEdge>

extension ControlFlowGraph {

  /// Inserts a control edge into the graph.
  mutating func insertControlEdge(from source: BasicBlockIndex, to target: BasicBlockIndex) {
    let (inserted, label) = insertEdge(from: source, to: target, labeledBy: .forward)
    if inserted {
      self[target, source] = .backward
    } else if label == .backward {
      self[source, target] = .bidirectional
      self[target, source] = .bidirectional
    }
  }

  /// Removes a control edge from the graph.
  mutating func removeControlEdge(from source: BasicBlockIndex, to target: BasicBlockIndex) {
    self[source, target] = nil
    self[target, source] = nil
  }

}

/// An edge in a control-flow graph.
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

  /// The VIL stage of a function.
  public struct Stage: OptionSet {

    public typealias RawValue = UInt8

    public let rawValue: UInt8

    public init(rawValue: UInt8) {
      self.rawValue = rawValue
    }

    static let didPassOwnership = Stage(rawValue: 1)

    static let checked: Stage = [.didPassOwnership]

    static let optimized: Stage = .checked

  }

  /// The mangled name of the function.
  public let name: String

  /// An optional debug name describing the function.
  public let debugName: String?

  /// The VIL type of the function.
  public let type: VILType

  /// The indices of the basic blocks in the function, starting with the entry.
  public internal(set) var blocks: [BasicBlockIndex] = []

  /// The control flow graph of the function.
  public internal(set) var cfg = ControlFlowGraph()

  /// The stage of the function.
  public var stage: Stage = []

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

}
