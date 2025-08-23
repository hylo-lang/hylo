import Utils

/// A region of the program rooted at a definition.
///
/// A lifetime rooted at a definition `d` is a region starting immediately after `d` and covering
/// a set of uses dominated by `d`. The "upper boundaries" of a lifetime are the program points
/// immediately after the uses sequenced last in that region.
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

  /// Creates an instance with the given properties.
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

  /// The upper boundaries of the region formed by the elements in `self`.
  ///
  /// There's one upper boundary per basic block covered in the lifetime that isn't live-out. It
  /// falls immediately after the last element of `self` also contained in that block, or, in the
  /// case of a live-in block with no use, immediately before the first instruction.
  var upperBoundaries: some Sequence<InsertionPoint> {
    coverage.lazy.compactMap { (b, c) -> InsertionPoint? in
      switch c {
      case .liveIn(let use):
        return use.map({ .after($0.user) }) ?? .start(of: Block.ID(b))
      case .closed(let use):
        return use.map({ .after($0.user) }) ?? .after(operand.instruction!)
      case .liveInAndOut, .liveOut:
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
  func liveRange(of operand: Operand, definedIn site: Block.ID, from f: Function.ID) -> Lifetime {

    // This implementation is a variant of Appel's path exploration algorithm found in Brandner et
    // al.'s "Computing Liveness Sets for SSA-Form Programs".

    // Find all blocks in which the operand is being used.
    var occurrences = functions[f]!.uses[operand, default: []].reduce(
      into: Set<Function.Blocks.Address>(),
      { (blocks, use) in blocks.insert(functions[f]!.block(of: use.user).address) })

    // Propagate liveness starting from the blocks in which the operand is being used.
    let cfg = functions[f]!.cfg()
    var approximateCoverage: [Function.Blocks.Address: (isLiveIn: Bool, isLiveOut: Bool)] = [:]
    while true {
      guard let occurrence = occurrences.popFirst() else { break }

      // `occurrence` is the defining block.
      if site.address == occurrence { continue }

      // We already propagated liveness to the block's live-in set.
      if approximateCoverage[occurrence]?.isLiveIn ?? false { continue }

      // Mark that the definition is live at the block's entry and propagate to its predecessors.
      approximateCoverage[occurrence, default: (false, false)].isLiveIn = true
      for predecessor in cfg.predecessors(of: occurrence) {
        approximateCoverage[predecessor, default: (false, false)].isLiveOut = true
        occurrences.insert(predecessor)
      }
    }

    var coverage: Lifetime.Coverage = [:]

    // If the operand isn't live out of its defining block, its last use is in that block.
    if approximateCoverage.isEmpty {
      coverage[site.address] = .closed(lastUse: lastUse(of: operand, in: site, from: f))
      return Lifetime(operand: operand, coverage: coverage)
    }

    // Find the last use in each block for which the operand is not live out.
    var successors: Set<Function.Blocks.Address> = []
    for (block, bounds) in approximateCoverage {
      switch bounds {
      case (true, true):
        coverage[block] = .liveInAndOut
        successors.formUnion(cfg.successors(of: block))
      case (false, true):
        coverage[block] = .liveOut
        successors.formUnion(cfg.successors(of: block))
      case (true, false):
        coverage[block] = .liveIn(lastUse: lastUse(of: operand, in: Block.ID(block), from: f))
      case (false, false):
        continue
      }
    }

    // Mark successors of live out blocks as live in if they haven't been already.
    for block in successors where coverage[block] == nil {
      coverage[block] = .liveIn(lastUse: nil)
    }

    return Lifetime(operand: operand, coverage: coverage)
  }

  /// Returns `l` in which `i` has been inserted.
  ///
  /// - Requires: The definition of `l` dominates `u`.
  func extend(lifetime l: Lifetime, toInclude u: Use, in f: Function.ID) -> Lifetime {
    var coverage = l.coverage
    switch coverage[functions[f]!.block(of: u.user).address] {
    case .closed(let lastUser):
      coverage[functions[f]!.block(of: u.user).address] = .closed(lastUse: last(lastUser, u, in: f))
    case .liveIn(let lastUser):
      coverage[functions[f]!.block(of: u.user).address] = .liveIn(lastUse: last(lastUser, u, in: f))
    default:
      break
    }

    return .init(operand: l.operand, coverage: coverage)
  }

  /// Returns `left` extended to cover the instructions in `right`.
  ///
  /// - Requires: `left` and `right` are defined in the same function, which is in `self`. The
  ///   operand for which `right` is defined must be in `left`.
  func extend(lifetime left: Lifetime, toCover right: Lifetime, in f: Function.ID) -> Lifetime {
    let coverage = left.coverage.merging(right.coverage) { (a, b) in
      switch (a, b) {
      case (.liveOut, .liveIn), (.liveIn, .liveOut):
        unreachable("definition does not dominate all uses")
      case (.liveInAndOut, _), (_, .liveInAndOut):
        return .liveInAndOut
      case (.liveOut, _), (_, .liveOut):
        return .liveOut
      case (.liveIn(let lhs), .liveIn(let rhs)):
        return .liveIn(lastUse: last(lhs, rhs, in: f))
      case (.liveIn(let lhs), .closed(let rhs)):
        return .liveIn(lastUse: last(lhs, rhs, in: f))
      case (.closed(let lhs), .liveIn(let rhs)):
        return .liveIn(lastUse: last(lhs, rhs, in: f))
      case (.closed(let lhs), .closed(let rhs)):
        return .closed(lastUse: last(lhs, rhs, in: f))
      }
    }
    return .init(operand: left.operand, coverage: coverage)
  }

  /// Returns the last use of `operand` in `block`.
  private func lastUse(of operand: Operand, in block: Block.ID, from f: Function.ID) -> Use? {
    let instructions = self[f].instructions(in: block)
    for i in instructions.reversed() {
      if let operandIndex = self[f][i].operands.lastIndex(of: operand) {
        return Use(
          user: i,
          index: operandIndex)
      }
    }

    // No use of `operand` in `block`.
    return nil
  }

  /// Returns the use that executes last.
  ///
  /// If the two uses are not in the same block, this returns `lhs`.
  private func last(_ lhs: Use?, _ rhs: Use?, in f: Function.ID) -> Use? {
    guard let lhs = lhs else { return rhs }
    guard let rhs = rhs else { return lhs }

    if lhs.user == rhs.user {
      return lhs.index < rhs.index ? rhs : lhs
    }

    if functions[f]!.precedes(lhs.user, rhs.user) {
      return rhs
    } else {
      return lhs
    }
  }

}
