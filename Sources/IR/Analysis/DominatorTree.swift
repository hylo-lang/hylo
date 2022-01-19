public struct DominatorTree {

  private enum Idom {

    case some(BasicBlock.ID)

    case missing

  }

  /// The function from which this tree was created.
  public let fun: VILFun

  /// Immediate dominators.
  private var idoms: [BasicBlock.ID: Idom]

  public init(fun: VILFun) {
    self.fun = fun
    self.idoms = [:]

    // Compute the immediate dominators of all basic blocks.
    guard fun.entryID != nil else { return }
    for b in fun.blocks.keys {
      _ = findIdom(b)
    }
  }

  /// A collection of the blocks of this tree in breadth-first order.
  public var breadthFirstBlocks: [BasicBlock.ID] {
    guard let entryID = fun.entryID else { return [] }

    let children: [BasicBlock.ID: [BasicBlock.ID]] = idoms.reduce(
      into: [:],
      { (children, pair) in
        if case .some(let parent) = pair.value {
          children[parent, default: []].append(pair.key)
        }
      })

    var results = [entryID]
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
  public func dominates(_ a: BasicBlock.ID, _ b: BasicBlock.ID) -> Bool {
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

  /// Returns `true` if the instruction at `defPath` dominates `use`.
  ///
  /// - Parameters:
  ///   - defPath: The path to an instruction.
  ///   - use: A use. `use` is assumed to reside in this tree's function.
  public func dominates(defPath: InstPath, use: Use) -> Bool {
    assert(defPath.funName == fun.name)

    // If `def` is in the same block as `use`, verify that it preceeds it.
    if defPath.blockID == use.userPath.blockID {
      for i in fun.blocks[defPath.blockID]!.instructions.indices {
        var instPath = defPath
        instPath.instIndex = i

        if instPath == defPath {
          return true
        } else if instPath == use.userPath {
          return false
        }
      }

      fatalError("'defPath' and 'use' do not refer to instructions in that function")
//      let defInst = fun[def]
//      for inst in fun.blocks[def.blockID]!.instructions {
//        if inst === defInst {
//          return true
//        } else if inst === fun[use.userPath] {
//          return false
//        }
//      }
    }

    // Verify that the block in which `def` resides dominates the block containing `use`.
    return dominates(defPath.blockID, use.userPath.blockID)
  }

  private mutating func findIdom(_ b: BasicBlock.ID) -> Idom {
    // Check the cache.
    if let idom = idoms[b] { return idom }

    // Build the set of predecessors.
    let preds = fun.cfg.edges(from: b).filter({ $0.label != .forward })
    if preds.isEmpty {
      idoms[b] = .missing
      return .missing
    }

    // If a block `b` has no predecessors, `b` is unreachable. If `b` has a single predecessor `a`,
    // `a` is its immediate dominator. If `b` has multiple direct predecessors, its immediate
    // dominator is the lowest common immediate dominator of all its direct predecessors.
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
      var chains = preds.compactMap({ (pred) -> [BasicBlock.ID]? in
        if pred.target == b { return nil }

        var chain: [BasicBlock.ID] = []
        var ancestor = pred.target
        while case .some(let a) = findIdom(ancestor) {
          if a == b { return nil }
          ancestor = a
          chain.append(ancestor)
        }

        // Keep the chain if we reached the entry; otherwise, throw it away.
        if ancestor == fun.entryID {
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
        var idom: BasicBlock.ID = fun.entryID!
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
