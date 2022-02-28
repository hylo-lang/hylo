import Utils

public struct DominatorTree {

  private enum Idom {

    case some(BasicBlockIndex)

    case missing

  }

  /// The control-flow graph of the function to which this tree relates.
  public let cfg: ControlFlowGraph

  /// The basic blocks of the function to which this tree relates.
  public let blocks: [BasicBlockIndex]

  /// Immediate dominators.
  private var idoms: [BasicBlockIndex: Idom]

  /// Creates a dominator tree from a control-flow graph.
  ///
  /// - Parameters
  ///   - cfg: The control-flow graph of a function.
  ///   - blocks: An array with all basic the basic blocks of a function, starting with its entry.
  public init(from cfg: ControlFlowGraph, blocks: [BasicBlockIndex]) {
    self.cfg = cfg
    self.blocks = blocks
    self.idoms = [:]

    // Compute the immediate dominators of all basic blocks.
    guard !blocks.isEmpty else { return }
    for b in blocks { _ = findIdom(b) }
  }

  /// Creates the dominator tree of the specified function.
  ///
  /// - Parameter fun: A VIL function.
  public init(of fun: VILFun) {
    self.init(from: fun.cfg, blocks: fun.blocks)
  }

  /// A collection of the blocks of this tree in breadth-first order.
  public var breadthFirstBlocks: [BasicBlockIndex] {
    guard let entry = blocks.first else { return [] }

    let children: [BasicBlockIndex: [BasicBlockIndex]] = idoms.reduce(
      into: [:],
      { (children, pair) in
        if case .some(let parent) = pair.value {
          children[parent, default: []].append(pair.key)
        }
      })

    var results = [entry]
    var i = 0
    while i < results.count {
      results.append(contentsOf: children[results[i], default: []])
      i += 1
    }
    return results
  }

  /// Returns `true` if the basic block identified by `a` dominates the block identified by `b`.
  ///
  /// Both basic blocks are assumed to reside in this tree's function.
  public func dominates(_ a: BasicBlockIndex, _ b: BasicBlockIndex) -> Bool {
    // By definition, every block dominates itself.
    if (a == b) { return true }

    // Walk the dominator tree from `b` up to the root to find `a`.
    var child = b
    while case .some(let parent) = idoms[child]! {
      if parent == a { return true }
      child = parent
    }
    return false
  }

  /// Returns `true` if the instruction at `def` dominates `use`.
  ///
  /// - Parameters:
  ///   - def: An instruction index.
  ///   - use: A use. `use` is assumed to reside in this tree's control-flow graph.
  ///   - module: The module in which `def` and `use` are defined.
  public func dominates(def: InstIndex, use: Use, in module: Module) -> Bool {
    // Look for the block in which `def` and `use` reside.
    let defBlock = module.instructions[def].parent
    let useBlock = module.instructions[use.user].parent

    // If `def` is in the same block as `use`, verify that it preceeds it.
    if defBlock == useBlock {
      for i in module.blocks[defBlock].instructions {
        if i == def {
          return true
        } else if i == use.user {
          return false
        }
      }
      fatalError("unreachable")
    }

    // Verify that the block in which `def` resides dominates the block containing `use`.
    return dominates(defBlock, useBlock)
  }

  private mutating func findIdom(_ b: BasicBlockIndex) -> Idom {
    // Check the cache.
    if let idom = idoms[b] { return idom }

    // Build the set of predecessors.
    let preds = cfg.edges(from: b).filter({ $0.label != .forward })
    if preds.isEmpty {
      idoms[b] = .missing
      return .missing
    }

    // If `b` has no predecessors, it is unreachable. If `b` has one single predecessor `a`, its
    // immediate dominator is `a`. If `b` has multiple direct predecessors, its immediate dominator
    // is is the lowest common immediate dominator of all its direct predecessors.
    switch preds.count {
    case 0:
      idoms[b] = .missing
      return .missing

    case 1:
      idoms[b] = .some(preds[0].target)
      return .some(preds[0].target)

    default:
      // For each incoming edge, build a chain of immediate dominators up to the entry, filtering
      // out chains that contain the original block or those start with an unreachable block. The
      // immediate dominator is is the first block of the longest common suffix of all remaining
      // chains. If there are no chains left, then all successors are unreachable and the block
      // simply doesn't have a dominator.
      var chains = preds.compactMap({ (pred) -> [BasicBlockIndex]? in
        if pred.target == b { return nil }

        var chain: [BasicBlockIndex] = []
        var ancestor = pred.target
        while case .some(let a) = findIdom(ancestor) {
          if a == b { return nil }
          ancestor = a
          chain.append(ancestor)
        }

        // Keep the chain if we reached the entry; otherwise, throw it away.
        if ancestor == blocks[0] {
          chain.append(ancestor)
          return chain
        } else {
          return nil
        }
      })
      assert(chains.allSatisfy({ !$0.isEmpty }))

      switch chains.count {
      case 0:
        idoms[b] = .missing
        return .missing

      case 1:
        idoms[b] = .some(chains[0][0])
        return .some(chains[0][0])

      default:
        var idom: BasicBlockIndex = blocks[0]
        outer:while let candidate = chains[0].popLast() {
          for i in 1 ..< chains.count {
            if chains[i].popLast() != candidate { break outer }
          }
          idom = candidate
        }

        idoms[b] = .some(idom)
        return .some(idom)
      }
    }
  }

}
