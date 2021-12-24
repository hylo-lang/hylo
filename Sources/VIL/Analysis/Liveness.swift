/// An analysis that computes the basic blocks where an SSA variable is live.
public struct LivenessAnalysis {

  /// Returns whether the analysis should compute its liveness.
  public let isOfInterest: (Inst) -> Bool

  private var fun: VILFun!

  private var result: LivenessResult = [:]

  /// Creates a new analysis pass.
  ///
  /// - Parameter isOfInterest:A closure that accepts a definition and returns whether the analysis
  ///   should compute its liveness. By default, all definitions are analyzed.
  public init(isOfInterest: @escaping (Inst) -> Bool) {
    self.isOfInterest = isOfInterest
  }

  /// Runs the analysis on the specified function.
  public mutating func run(on funName: String, in module: inout Module) -> LivenessResult {
    fun = module.functions[funName]!
    result = Dictionary(uniqueKeysWithValues: fun.blocks.map({
      ($0, LivenessSets(defs: [], liveIn: [], liveOut: []))
    }))

    for block in fun.blocks {
      for inst in module.blocks[block].instructions {
        // Check if the analysis should process the definition.
        let def = module.instructions[inst]
        guard isOfInterest(def) else { continue }

        // Register the definition.
        result[block]!.defs.insert(inst)

        // Compute the liveness sets using the def-use chain.
        if let uses = module.uses[Operand(inst)] {
          for use in uses {
            let user = module.instructions[use.user]
            mark(inst: inst, block: user.parent, in: &module)
          }
        }
      }
    }

    return result
  }

  private mutating func mark(inst: InstIndex, block: BasicBlockIndex, in module: inout Module) {
    // Stop if the instruction is defined in the block.
    if module.instructions[inst].parent == block {
      result[block]!.defs.insert(inst)
      return
    }

    // Stop if we already propagated liveness to the block's live-in set.
    let (inserted, _) = result[block]!.liveIn.insert(inst)
    if !inserted { return }

    // Propagate liveness to the block's predecessors.
    for edge in fun.cfg.edges(to: block) {
      result[edge.source]!.liveOut.insert(inst)
      mark(inst: inst, block: edge.source, in: &module)
    }
  }

}

public typealias LivenessResult = [BasicBlockIndex: LivenessSets]

/// The liveness sets of a basic block.
public struct LivenessSets {

  /// The set of SSA variables that are defined by the block.
  public var defs: Set<InstIndex>

  /// The set of SSA variables that are live on entry to the block.
  public var liveIn: Set<InstIndex>

  /// The set of SSA variables that are live on exit froms the block.
  public var liveOut: Set<InstIndex>

}
