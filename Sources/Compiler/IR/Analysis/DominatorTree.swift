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

  /// The parent of a node.
  private enum Dominator {

    case missing

    case present(Node)

  }

  /// The root of the tree.
  let root: Node

  /// The immediate dominators of each basic block.
  ///
  /// - Note: The value of the dictionary is a custom-defined optional so that we may distinguish
  ///   `nil` from `Dominator.missing` when subscripting it.
  private var immediateDominators: [Node: Dominator]

  /// Creates the dominator tree of the specified function.
  init(function functionID: Function.ID, cfg: ControlFlowGraph? = nil, in module: Module) {
    let function = module[function: functionID]
    let cfg = cfg ?? function.cfg

    root = function.blocks.firstAddress!
    immediateDominators = [:]
    immediateDominators.reserveCapacity(function.blocks.count)
    for i in function.blocks.indices {
      _ = findImmediateDominator(i.address, cfg: cfg, blocks: function.blocks)
    }
  }

  /// A collection containing the blocks in this tree in breadth-first order.
  var bfs: [Node] {
    let children: [Node: [Node]] = immediateDominators.reduce(
      into: [:],
      { (children, kv) in
        if case .present(let parent) = kv.value {
          children[parent, default: []].append(kv.key)
        }
      })

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
    if case .present(let b) = immediateDominators[block]! {
      return b
    } else {
      return nil
    }
  }

  /// Returns a collection containing the strict dominators of `block`.
  func strictDominators(of block: Node) -> [Node] {
    var result: [Node] = []
    var a = block
    while case .present(let b) = immediateDominators[a]! {
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
    while case .present(let parent) = immediateDominators[child]! {
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
      for i in module[function: definition.function][definition.block].instructions.indices {
        if i.address == definition.address {
          return true
        }
      }
      return false
    }

    // Return whether the block containing `definition` dominates the block containing `use`.
    return dominates(definition.block, use.user.block)
  }

  private mutating func findImmediateDominator(
    _ node: Node,
    cfg: ControlFlowGraph,
    blocks: DoublyLinkedList<Block>
  ) -> Dominator {
    // Check the cache.
    if let dominator = immediateDominators[node] { return dominator }

    // Build the set of predecessors.
    let predecessors = cfg.predecessors(of: node)

    switch predecessors.count {
    case 0:
      // If `node` has no predecessors, it has no dominator.
      immediateDominators[node] = .missing
      return .missing

    case 1:
      // If `node` has one single predecessor, its immediate dominator is that predecessor.
      immediateDominators[node] = .present(predecessors[0])
      return .present(predecessors[0])

    default:
      // If `node` has multiple direct predecessors, its immediate dominator is is the closest
      // common immediate dominator of all its direct predecessors.

      // For each incoming edge, build a chain of immediate dominators up to the entry, filtering
      // out chains that contain the original block or those start with an unreachable block. The
      // immediate dominator is is the first block of the longest common suffix of all remaining
      // chains. If there are no chains left, then all successors are unreachable and the block
      // simply doesn't have a dominator.
      var chains = predecessors.compactMap({ (predecessor) -> [Node]? in
        if predecessor == node { return nil }

        var chain: [Node] = []
        var ancestor = predecessor
        while case .present(let a) = findImmediateDominator(ancestor, cfg: cfg, blocks: blocks) {
          if a == node { return nil }
          ancestor = a
          chain.append(ancestor)
        }

        // Keep the chain if we reached the entry; otherwise, throw it away.
        if ancestor == root {
          chain.append(ancestor)
          return chain
        } else {
          return nil
        }
      })
      assert(chains.allSatisfy({ !$0.isEmpty }))

      switch chains.count {
      case 0:
        immediateDominators[node] = .missing
        return .missing

      case 1:
        immediateDominators[node] = .present(chains[0][0])
        return .present(chains[0][0])

      default:
        var dominator = root
        outer: while let candidate = chains[0].popLast() {
          for i in 1..<chains.count {
            if chains[i].popLast() != candidate { break outer }
          }
          dominator = candidate
        }

        immediateDominators[node] = .present(dominator)
        return .present(dominator)
      }
    }
  }

}
