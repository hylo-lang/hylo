import Utils

/// The lifetime of an operand.
///
/// The lifetime of an operand `o` is the set containing all instructions using `o`, partially
/// ordered by the function's evaluation order. The maximal elements of `o`'s lifetime are the its
/// last users.
///
/// - Note: The definition of an operand `o` isn't part of `o`'s lifetime.
struct Lifetime {

  fileprivate typealias Coverage = [Function.Blocks.Address: BlockCoverage]

  /// A data structure encoding how a block covers the lifetime.
  enum BlockCoverage {

    /// The operand is live in and out of the block.
    case liveInAndOut

    /// The operand is only live out.
    case liveOut

    /// The operand is only live in. The payload is its last use, if any.
    case liveIn(lastUse: Use?)

    /// The operand is neither live in or out, but it's used in the block. The payload is its last
    /// use, if any.
    case closed(lastUse: Use?)

  }

  /// The operand whose `self` is the lifetime.
  ///
  /// - Note: `operand` is either an instruction or a basic block parameter.
  let operand: Operand

  /// The set of instructions in the lifetime.
  fileprivate let coverage: Coverage

  /// Creates an empty lifetime.
  init(operand: Operand) {
    self.operand = operand
    self.coverage = [:]
  }

  fileprivate init(operand: Operand, coverage: Coverage) {
    self.operand = operand
    self.coverage = coverage
  }

  /// Indicates whether the lifetime is empty.
  var isEmpty: Bool {
    for blockCoverage in coverage.values {
      switch blockCoverage {
      case .liveInAndOut, .liveOut, .liveIn, .closed(lastUse: .some):
        return false
      default:
        continue
      }
    }
    return true
  }

  /// Returns the instructions in `self` that do not preceed any other instruction in `self`.
  func maximalElements() -> [Use] {
    coverage.values.compactMap() { (blockCoverage) in
      switch blockCoverage {
      case .liveIn(let use):
        return use
      case .closed(let use):
        return use
      default:
        return nil
      }
    }
  }

}

extension Module {

  /// Given `operand` is an instruction or block parameter, returns its live-range.
  ///
  /// The live-range `L` of an operand `x` in a function `f` is the minimal lifetime such that for
  /// for all instructions `i` in `f`, if `i` uses `x` then `i` is in `L`.
  func liveSite(of operand: Operand, definedIn site: Block.ID) -> Lifetime {
    // Note: the search implements a variant of Appel's path exploration algorithm, found in
    // "Computing Liveness Sets for SSA-Form Programs" by Brandner et al.

    // Find all blocks in which the operand is being used.
    var occurences = uses[operand, default: []].reduce(
      into: Set<Function.Blocks.Address>(),
      { (blocks, use) in blocks.insert(use.user.block) })

    // Propagate liveness starting from the blocks in which the operand is being used.
    let cfg = functions[site.function]!.cfg()
    var approximateCoverage: [Function.Blocks.Address: (isLiveIn: Bool, isLiveOut: Bool)] = [:]
    while true {
      guard let occurence = occurences.popFirst() else { break }

      // `occurence` is the defining block.
      if site.address == occurence { continue }

      // We already propagated liveness to the block's live-in set.
      if approximateCoverage[occurence]?.isLiveIn ?? false { continue }

      // Mark that the definition is live at the block's entry and propagate to its predecessors.
      approximateCoverage[occurence, default: (false, false)].isLiveIn = true
      for predecessor in cfg.predecessors(of: occurence) {
        approximateCoverage[predecessor, default: (false, false)].isLiveOut = true
        occurences.insert(predecessor)
      }
    }

    // Refine the coverage.
    var coverage: Lifetime.Coverage = [:]

    // If the operand isn't live out of its defining block, its last use is in that block.
    if approximateCoverage.isEmpty {
      coverage[site.address] = .closed(lastUse: lastUse(of: operand, in: site))
      return Lifetime(operand: operand, coverage: coverage)
    }

    // Find the last use in each block for which the operand is not live out.
    for (block, bounds) in approximateCoverage {
      switch bounds {
      case (true, true):
        coverage[block] = .liveInAndOut
      case (false, true):
        coverage[block] = .liveOut
      case (true, false):
        let id = Block.ID(function: site.function, address: block)
        coverage[block] = .liveIn(lastUse: lastUse(of: operand, in: id))
      case (false, false):
        continue
      }
    }

    return Lifetime(operand: operand, coverage: coverage)
  }

  /// Returns `left` extended to cover the instructions in `right`.
  ///
  /// - Requires: `left` and `right` must be defined in the same function, which must be defined in
  ///   `self`. The operand for which `right` is defined must be in `left`.
  func extend(lifetime left: Lifetime, with right: Lifetime) -> Lifetime {
    precondition(left.operand.function! == right.operand.function!)

    /// Returns the use that executes last.
    func last(_ lhs: Use?, _ rhs: Use?) -> Use? {
      guard let lhs = lhs else { return rhs }
      guard let rhs = rhs else { return lhs }

      if lhs.user == rhs.user {
        return lhs.index < rhs.index ? rhs : lhs
      }

      let block = functions[lhs.user.function]![lhs.user.block]
      if lhs.user.address.precedes(rhs.user.address, in: block.instructions) {
        return rhs
      } else {
        return lhs
      }
    }

    let coverage = left.coverage.merging(
      right.coverage,
      uniquingKeysWith: { (a, b) in
        switch (a, b) {
        case (.liveOut, .liveIn), (.liveIn, .liveOut):
          unreachable("definition does not dominate all uses")
        case (.liveInAndOut, _), (_, .liveInAndOut):
          return .liveInAndOut
        case (.liveOut, _), (_, .liveOut):
          return .liveOut
        case (.liveIn(let lhs), .liveIn(let rhs)):
          return .liveIn(lastUse: last(lhs, rhs))
        case (.liveIn(let lhs), .closed(let rhs)):
          return .liveIn(lastUse: last(lhs, rhs))
        case (.closed(let lhs), .liveIn(let rhs)):
          return .liveIn(lastUse: last(lhs, rhs))
        case (.closed(let lhs), .closed(let rhs)):
          return .liveIn(lastUse: last(lhs, rhs))
        }
      })
    return Lifetime(operand: left.operand, coverage: coverage)
  }

  /// Returns the last use of `operand` in `block`.
  private func lastUse(of operand: Operand, in block: Block.ID) -> Use? {
    let instructions = functions[block.function]![block.address].instructions
    for i in instructions.indices.reversed() {
      if let operandIndex = instructions[i].operands.lastIndex(of: operand) {
        return Use(
          user: InstructionID(block.function, block.address, i.address),
          index: operandIndex)
      }
    }

    // No use of `operand` in `block`.
    return nil
  }

}
