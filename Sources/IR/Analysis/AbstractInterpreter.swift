import DequeModule

struct AbstractInterpreter<Domain: AbstractDomain> {

  /// An abstract interpretation context.
  typealias Context = AbstractContext<Domain>

  /// A map fron function block to the context of the abstract interpreter before and after the
  /// evaluation of its instructions.
  typealias Contexts = [Function.Blocks.Address: (before: Context, after: Context)]

  /// A closure that processes `block` in `context`, given an abstract `machine`.
  typealias Interpret = (
    _ block: Function.Blocks.Address,
    _ machine: inout Self,
    _ context: inout Context
  ) -> Void

  /// The function being interpreted.
  private let subject: Function.ID

  /// The control flow graph of the function being interpreted.
  private var cfg: ControlFlowGraph

  /// The dominator tree of the function being interpreted.
  private var dominatorTree: DominatorTree

  /// The state of the abstract interpreter before and after the visited basic blocks.
  private var contexts: Contexts = [:]

  /// A FILO list of blocks to visit.
  private var work: Deque<Function.Blocks.Address>

  /// The set of blocks that no longer need to be visited.
  private var done: Set<Function.Blocks.Address>

  /// Creates an interpreter analyzing `f` which is in `m`, starting with `entryContext`.
  init(
    analyzing f: Function.ID,
    in m: Module,
    entryContext: Context
  ) {
    self.subject = f
    self.cfg = m[f].cfg()
    self.dominatorTree = DominatorTree(function: f, cfg: cfg, in: m)
    self.contexts = [m[f].entry!: (before: entryContext, after: Context())]
    self.work = Deque(dominatorTree.bfs)
    self.done = []
  }

  /// Recomputes the CFG of the function being interpreted.
  ///
  /// Call this method to update the control-flow information used by this instance to guide
  /// abstract interpretation.
  mutating func recomputeControlFlow(_ m: Module) {
    cfg = m[subject].cfg()
    dominatorTree = .init(function: subject, cfg: cfg, in: m)
  }

  /// Removes `b` from the work list.
  ///
  /// - Requires: `b` is in the work list.
  mutating func removeWork(_ b: Function.Blocks.Address) {
    work.remove(at: work.firstIndex(of: b)!)
  }

  /// Runs this instance using `interpret` to interpret basic blocks.
  mutating func fixedPoint(_ interpret: Interpret) {
    while let blockToProcess = work.popFirst() {
      guard isVisitable(blockToProcess) else {
        work.append(blockToProcess)
        continue
      }

      let (before, sources) = beforeContext(of: blockToProcess)

      let after: Context
      if (blockToProcess == dominatorTree.root) || (contexts[blockToProcess]?.before != before) {
        after = afterContext(of: blockToProcess, in: before, processingBlockWith: interpret)
      } else if sources.count != cfg.predecessors(of: blockToProcess).count {
        after = contexts[blockToProcess]!.after
      } else {
        done.insert(blockToProcess)
        continue
      }

      // We're done with the current block if ...
      let isBlockDone: Bool = {
        // 1) we're done with all of the block's predecessors.
        let pending = cfg.predecessors(of: blockToProcess).filter({ !done.contains($0) })
        if pending.isEmpty { return true }

        // 2) the only predecessor left is the block itself, yet the after-context didn't change.
        return (pending.count == 1)
          && (pending[0] == blockToProcess)
          && (contexts[blockToProcess]?.after == after)
      }()

      // Update the before/after-context pair for the current block and move to the next one.
      contexts[blockToProcess] = (before: before, after: after)
      if isBlockDone {
        done.insert(blockToProcess)
      } else {
        work.append(blockToProcess)
      }
    }
  }

  /// Returns `true` if `b` has been visited.
  private func visited(_ b: Function.Blocks.Address) -> Bool {
    contexts[b] != nil
  }

  /// Returns `true` if `b` is ready to be visited.
  ///
  /// Computing the before-context of `b` requires knowing the state of all uses in `b` that are
  /// defined its (transitive) predecessors. Because a definition must dominate all its uses, we
  /// can assume the predecessors dominated by `b` don't define variables used in `b`. Hence, `b`
  /// can be visited iff all its predecessors have been visited or are dominated by `b`.
  private func isVisitable(_ b: Function.Blocks.Address) -> Bool {
    if let d = dominatorTree.immediateDominator(of: b) {
      return visited(d)
        && cfg.predecessors(of: b).allSatisfy({ (p) in
          visited(p) || dominatorTree.dominates(b, p)
        })
    } else {
      // No predecessor.
      return true
    }
  }

  /// Returns the before-context of `b` and the predecessors from which it's been computed.
  ///
  /// - Requires: `isVisitable(b)` is `true`
  private func beforeContext(
    of b: Function.Blocks.Address
  ) -> (context: Context, sources: [Function.Blocks.Address]) {
    if b == dominatorTree.root {
      return (contexts[b]!.before, [])
    }

    let p = cfg.predecessors(of: b)
    let sources = p.filter({ contexts[$0] != nil })
    return (.init(merging: sources.lazy.map({ contexts[$0]!.after })), sources)
  }

  /// Returns the after-context of `b` by processing it with `interpret` in `initialContext`.
  mutating func afterContext(
    of b: Function.Blocks.Address,
    in initialContext: Context,
    processingBlockWith interpret: Interpret
  ) -> Context {
    var newContext = initialContext
    interpret(b, &self, &newContext)
    return newContext
  }
  
}
