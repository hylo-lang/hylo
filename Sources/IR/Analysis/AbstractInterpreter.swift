import DequeModule

struct AbstractInterpreter<Domain: AbstractDomain> {

  /// An abstract interpretation context.
  typealias Context = AbstractContext<Domain>

  /// The knowledge of the abstract interpreter about a single block.
  typealias BlockState = (sources: Set<Function.Blocks.Address>, before: Context, after: Context)

  /// A map fron function block to the context of the abstract interpreter before and after the
  /// evaluation of its instructions.
  typealias State = [Function.Blocks.Address: BlockState]

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
  private var state: State = [:]

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
    self.state = [m[f].entry!: (sources: [], before: entryContext, after: Context())]
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

      // Interpret the block's IR unless we already reached a fixed point.
      let (sources, before) = beforeContext(of: blockToProcess)
      if let s = state[blockToProcess],
        blockToProcess != dominatorTree.root,
        s.sources == sources && s.before == before
      {
        done.insert(blockToProcess)
        continue
      }

      let after = afterContext(of: blockToProcess, in: before, processingBlockWith: interpret)
      state[blockToProcess] = (sources: sources, before: before, after: after)
      if sources.count == cfg.predecessors(of: blockToProcess).count {
        done.insert(blockToProcess)
      } else {
        work.append(blockToProcess)
      }
    }
  }

  /// Returns `true` if `b` has been visited.
  private func visited(_ b: Function.Blocks.Address) -> Bool {
    state[b] != nil
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
  ) -> (sources: Set<Function.Blocks.Address>, before: Context) {
    if b == dominatorTree.root {
      return ([], state[b]!.before)
    }

    let sources = Set(cfg.predecessors(of: b).filter({ state[$0] != nil }))
    return (sources, .init(merging: sources.lazy.map({ state[$0]!.after })))
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
