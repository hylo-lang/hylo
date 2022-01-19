import AST
import Basic

public enum ControlEdge {

  case forward, backward, bidirectional

}

public typealias ControlFlowGraph = AdjacencyList<BasicBlock.ID, ControlEdge>

/// A VIL function.
public struct VILFun {

  /// An iterator that supplies pairs `(path, inst)`, where path is an instruction path and `inst`
  /// the instruction at that path, for all the instructions in a function.
  public struct InstEnumerator: IteratorProtocol {

    fileprivate let funName: VILName

    fileprivate var blocks: [(key: BasicBlock.ID, value: BasicBlock)]

    fileprivate var instIndex: BasicBlock.Index?

    fileprivate init(funName: VILName, blocks: [(key: BasicBlock.ID, value: BasicBlock)]) {
      self.funName = funName
      self.blocks = blocks
      self.instIndex = blocks.last?.value.instructions.startIndex
    }

    public mutating func next() -> (path: InstPath, inst: Inst)? {
      while let (blockID, block) = blocks.last, let i = instIndex {
        if i != block.instructions.endIndex {
          defer { instIndex = block.instructions.index(after: i) }
          return (
            path: InstPath(funName: funName, blockID: blockID, instIndex: i),
            inst: block.instructions[i])
        } else {
          blocks.removeLast()
          instIndex = blocks.last?.value.instructions.startIndex
        }
      }
      return nil
    }

  }

  /// The mangled name of the function.
  public let name: VILName

  /// An optional debug name describing the function.
  public let debugName: String?

  /// The VIL type of the function.
  public let type: VILFunType

  /// The basic blocks of the function.
  ///
  /// Do not add or remove blocks using this property. Use `createBlock(arguments:before:)` or
  /// `removeBasicBlock(_:)` instead.
  public var blocks: [BasicBlock.ID: BasicBlock] = [:]

  /// The identifier of the entry block.
  public var entryID: BasicBlock.ID?

  /// The control flow graph of the function.
  ///
  /// This graph describes the relation between the basic blocks of the function. The direction of
  /// of its edges denotes the direction of the control flow from one block to another: there an
  /// edge from `A` to `B` if the former's terminator points to the latter.
  ///
  /// The graph is represented as an adjacency list that encodes the successor and the predecessor
  /// relations, enabling efficient lookup in both directions. Let `l` be the label on an edge
  /// from `A` to `B`:
  /// - if `l = .forward`, then `A` is a predecessor of `B`;
  /// - if `l = .backward`, then `A` is a successor of `B`;
  /// - if `l = .bidirectional`, then `A` is a predecessor *and* a successor of `B`.
  public var cfg = ControlFlowGraph()

  /// Creates a new VIL function.
  ///
  /// - Parameters:
  ///   - name: The name of the function.
  ///   - type: The unapplied type of the function.
  ///   - debugName: An optional debug name describing the function.
  init(name: VILName, type: VILFunType, debugName: String? = nil) {
    self.name = name
    self.debugName = debugName
    self.type = type
  }

  /// A Boolean value that indicates whether the function has an entry.
  public var hasEntry: Bool { entryID != nil }

  /// The entry block of the function.
  public var entry: BasicBlock? {
    guard let entryID = self.entryID else { return nil }
    return blocks[entryID]
  }

  /// Returns an iterator that supplies all instructions of the function.
  ///
  /// The iterator offers no guarantee over the order in which it returns the function's
  /// instructions.
  public func makeInstIterator() -> JoinedIterator<StableDoublyLinkedList<Inst>.Iterator> {
    return JoinedIterator(blocks.values.map({ $0.instructions.makeIterator() }))
  }

  /// Returns an iterator that supplies all instructions of the function, together with their path.
  ///
  /// The iterator offers no guarantee over the order in which instructions are returned.
  public func makeInstEnumerator() -> InstEnumerator {
    return InstEnumerator(funName: name, blocks: Array(blocks))
  }

  /// Dereference an instruction path, assuming it refers into this function.
  public subscript(path: InstPath) -> Inst {
    assert(path.funName == name)
    return blocks[path.blockID]!.instructions[path.instIndex]
  }

  /// Inserts a control edge from one basic block to another.
  mutating func insertControlEdge(from source: BasicBlock.ID, to target: BasicBlock.ID) {
    assert(blocks[source] != nil && blocks[target] != nil)

    let (inserted, label) = cfg.insertEdge(from: source, to: target, labeledBy: .forward)
    if inserted {
      cfg[target, source] = .backward
    } else if label == .backward {
      cfg[source, target] = .bidirectional
      cfg[target, source] = .bidirectional
    }
  }

}
