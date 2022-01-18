// Implementation note: The search for the last user of an operand implements a variant of Appel's
// path exploration algorithm, suggested by Brandner et al.'s research report "Computing Liveness
// Sets for SSA-Form Programs".

/// Searches for the last users of the specified instruction.
public func lastUsers(
  of inst: InstIndex,
  in module: Module
) -> [InstIndex] {
  let operand = Operand(inst)
  guard let uses = module.uses[operand] else { return [] }

  let block = module.instructions[inst].parent
  let fun = module.functions[module.blocks[block].parent]!
  return lastUsers(of: operand, definedInBlock: block, uses: uses, fun: fun, module: module)
}

/// Searches for the last users of the specified argument.
public func lastUsers(
  of argument: ArgValue,
  definedInBlock block: BasicBlockIndex,
  in module: Module
) -> [InstIndex] {
  let operand = Operand(argument)
  guard let uses = module.uses[operand] else { return [] }

  let fun = module.functions[module.blocks[block].parent]!
  return lastUsers(of: operand, definedInBlock: block, uses: uses, fun: fun, module: module)
}

fileprivate func lastUsers(
  of operand: Operand,
  definedInBlock block: BasicBlockIndex,
  uses: [Use],
  fun: VILFun,
  module: Module
) -> [InstIndex] {
  // Compute the live-range of the operand.
  let results = liveRange(of: operand, definedInBlock: block, uses: uses, module: module)

  // If the operand isn't live out of its defining block, its last user is in that block.
  if results.isEmpty {
    let list = module.blocks[block].instructions
    let last = list.last(where: { index in
      module.instructions[index].operands.contains(operand)
    })

    if let index = last {
      return [index]
    } else {
      assert(operand.argument != nil)
      return []
    }
  }

  // Find the last user in each block.
  return results.compactMap({ (blockIndex, result) -> InstIndex? in
    guard !result.isLiveOut else { return nil }
    let list = module.blocks[blockIndex].instructions
    return list.last(where: { index in
      module.instructions[index].operands.contains(operand)
    })
  })
}

/// Computes the live-range of the the specified operand for the given a set of uses.
public func liveRange(
  of operand: Operand,
  definedInBlock block: BasicBlockIndex,
  uses: [Use],
  module: Module
) -> [BasicBlockIndex: (isLiveIn: Bool, isLiveOut: Bool)] {
  // Find all blocks in which the operand is being used.
  var blocks = Set(uses.map({ (use: Use) -> BasicBlockIndex in
    module.instructions[use.user].parent
  }))

  // Propagate liveness starting from the blocks in which the operand is being used.
  let fun = module.functions[module.blocks[block].parent]!
  var results: [BasicBlockIndex: (isLiveIn: Bool, isLiveOut: Bool)] = [:]
  markLive(definitionInBlock: block, in: &blocks, cfg: fun.cfg, module: module, results: &results)

  return results
}

fileprivate func markLive(
  definitionInBlock parent: BasicBlockIndex,
  in blocks: inout Set<BasicBlockIndex>,
  cfg: ControlFlowGraph,
  module: Module,
  results: inout [BasicBlockIndex: (isLiveIn: Bool, isLiveOut: Bool)]
) {
  guard let block = blocks.popFirst() else { return }

  // Stop if the instruction is defined in the block.
  if parent == block {
    return markLive(
      definitionInBlock: parent, in: &blocks, cfg: cfg, module: module, results: &results)
  }

  // Stop if we already propagated liveness to the block's live-in set.
  if results[block]?.isLiveIn == true {
    return markLive(
      definitionInBlock: parent, in: &blocks, cfg: cfg, module: module, results: &results)
  }

  // Mark that the definition is live at the block's entry and propagate to its predecessors.
  results[block, default: (false, false)].isLiveIn = true
  for pred in cfg.predecessors(of: block) {
    results[pred, default: (false, false)].isLiveOut = true
    blocks.insert(pred)
  }

  return markLive(
    definitionInBlock: parent, in: &blocks, cfg: cfg, module: module, results: &results)
}
