import Utils

/// A tree whose nodes are basic blocks and where a node immediately dominates its children.
///
/// Definitions:
/// - A block `b1` in a control-flow graph *dominates* a block `b2` if every path from the entry to
///   `b2` must go through `b1`. By definition, every node dominates itself.
/// - A block `b1` *strictly dominates* a block `b2` if `b1` dominates `b2` and `b1 != b2`.
/// - A block `b1` is the *immediately dominates* a block `b2` if `b1` stricly dominates `b2` and
///   there is no block `b3` that stricly dominates `b2`.
///
/// A dominator tree encodes the dominance relation of a control graph as a tree where a node is
/// a basic blocks and its children are those it immediately dominates.
struct DominatorTree {

  /// A node in the tree.
  typealias Node = Function.Blocks.Address

  /// The root of the tree.
  let root: Node

  /// The immediate dominators of each basic block.
  private var immediateDominators: [Node: Node?]

  /// Creates the dominator tree of the specified function.
  init(function functionID: Function.ID, cfg: ControlFlowGraph? = nil, in module: Module) {
    let function = module[functionID]
    let cfg = cfg ?? function.cfg()

    root = function.entry!
    immediateDominators = [:]
    immediateDominators.reserveCapacity(function.blocks.count)
    for i in function.blocks.indices {
      _ = immediateDominator(of: i.address, in: cfg)
    }
  }

  /// A collection containing the blocks in this tree in breadth-first order.
  var bfs: [Node] {
    let children: [Node: [Node]] = immediateDominators.reduce(into: [:]) { (children, kv) in
      if case .some(let parent) = kv.value {
        children[parent, default: []].append(kv.key)
      }
    }

    var result = [root]
    var i = 0
    while i < result.count {
      if let nodes = children[result[i]] { result.append(contentsOf: nodes) }
      i += 1
    }
    return result
  }

  /// Returns the immediate dominator of `block`, if any.
  func immediateDominator(of block: Node) -> Node? {
    if case .some(let b) = immediateDominators[block]! {
      return b
    } else {
      return nil
    }
  }

  /// Returns a collection containing the strict dominators of `block`.
  func strictDominators(of block: Node) -> [Node] {
    var result: [Node] = []
    var a = block
    while case .some(let b) = immediateDominators[a]! {
      result.append(b)
      a = b
    }
    return result
  }

  /// Returns `true` if `a` dominates `b`.
  func dominates(_ a: Node, _ b: Node) -> Bool {
    // By definition, a node dominates itself.
    if a == b { return true }

    // Walk the dominator tree from `b` up to the root to find `a`.
    var child = b
    while case .some(let parent) = immediateDominators[child]! {
      if parent == a { return true }
      child = parent
    }
    return false
  }

  /// Returns `true` if the instruction identified by `definition` dominates `use`.
  ///
  /// - Requires: `definition` and `use` reside in the function associated with the true.
  func dominates(definition: InstructionID, use: Use, in module: Module) -> Bool {
    // If `definition` is in the same block as `use`, check which comes first.
    if definition.block == use.user.block {
      for i in module[definition.function][definition.block].instructions.indices {
        if i.address == definition.address {
          return true
        }
      }
      return false
    }

    // Return whether the block containing `definition` dominates the block containing `use`.
    return dominates(definition.block, use.user.block)
  }

  private mutating func immediateDominator(
    of node: Node,
    in cfg: ControlFlowGraph
  ) -> Node? {
    if let dominator = immediateDominators[node] { return dominator }

    let predecessors = cfg.predecessors(of: node)
    switch predecessors.count {
    case 0:
      // If `node` has no predecessors, it has no dominator.
      immediateDominators[node] = .some(nil)
      return nil

    case 1:
      // If `node` has one single predecessor, its immediate dominator is that predecessor.
      immediateDominators[node] = .some(predecessors[0])
      return predecessors[0]

    default:
      // If `node` has multiple direct predecessors, its immediate dominator is is the closest
      // common immediate dominator of all its direct predecessors.
      let d = cfg
        .paths(to: node, from: root)
        .firstCommonAncestor()
      immediateDominators[node] = .some(d)
      return d
    }
  }

}
