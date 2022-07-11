extension Module {

  private typealias LiveRange = [Function.BlockAddress: (isLiveIn: Bool, isLiveOut: Bool)]

  /// Given `operand` is an instruction or block parameter, returns its last uses.
  func lastUses(of operand: Operand) -> [Use] {
    // Note: the search implements a variant of Appel's path exploration algorithm, found in
    // "Computing Liveness Sets for SSA-Form Programs" by Brandner et al.

    // Retrieve the block in which the operand is defined.
    let origin: Block.ID
    switch operand {
    case .inst(let inst):
      origin = Block.ID(function: inst.function, address: inst.block)
    case .parameter(let block, _):
      origin = block
    case .constant:
      return []
    }

    // Compute the live-range of the operand.
    let liveRange = computeLiveRange(of: operand, definedIn: origin)

    // If the operand isn't live out of its defining block, its last use is in that block.
    if liveRange.isEmpty {
      if let use = lastUse(of: operand, in: origin) {
        return [use]
      } else {
        return []
      }
    }

    // Find the last use in each block for which the operand is not live out.
    var result: [Use] = []
    for (blockIndex, range) in liveRange where !range.isLiveOut {
      let block = Block.ID(function: origin.function, address: blockIndex)
      if let use = lastUse(of: operand, in: block) {
        result.append(use)
      }
    }

    return result
  }

  /// Returns the last use of `operand` in `block`.
  private func lastUse(of operand: Operand, in block: Block.ID) -> Use? {
    let instructions = functions[block.function][block.address].instructions
    for i in instructions.indices.reversed() {
      if let operandIndex = instructions[i].operands.lastIndex(of: operand) {
        return Use(
          user: InstID(function: block.function, block: block.address, address: i.address),
          index: operandIndex)
      }
    }

    // No use of `operand` in `block`.
    return nil
  }

  /// Given `operand` is an instruction or block parameter, returns its live-range.
  private func computeLiveRange(of operand: Operand, definedIn origin: Block.ID) -> LiveRange {
    // Find all blocks in which the operand is being used.
    var occurences = uses[operand, default: []].reduce(
      into: Set<Function.BlockAddress>(),
      { (blocks, use) in blocks.insert(use.user.block) })

    // Propagate liveness starting from the blocks in which the operand is being used.
    let cfg = functions[origin.function].cfg
    var result: LiveRange = [:]
    while true {
      guard let occurence = occurences.popFirst() else { break }

      // `occurence` is the defining block.
      if origin.address == occurence { continue }

      // We already propagated liveness to the block's live-in set.
      if result[occurence]?.isLiveIn ?? false { continue }

      // Mark that the definition is live at the block's entry and propagate to its predecessors.
      result[occurence, default: (false, false)].isLiveIn = true
      for predecessor in cfg.predecessors(of: occurence) {
        result[predecessor, default: (false, false)].isLiveOut = true
        occurences.insert(predecessor)
      }
    }

    return result
  }

}
