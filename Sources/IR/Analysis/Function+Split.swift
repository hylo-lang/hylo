extension Function {

  /// Splits `a` at all the points where `isBoundary` returns `true`, returning the parts.
  ///
  /// - Requires:
  ///   * `!a.isEmpty`.
  ///   * `!isSplit(a.last!)`
  ///   * if `isSplit` returns `true` for `n` instructions, the returned array has
  ///   `n + 1` elements.
  /// - Postcondition: all elements in `a` appear in exactly one part of the returned array.
  /// - Postcondition: if `p` is the last returned part, `p.splitPoint` is `nil` and `p.tail` is empty.
  internal func split(
    instructions a: [InstructionID], where isSplit: (InstructionID) -> Bool
  ) -> [SplitPart] {
    precondition(!a.isEmpty)
    precondition(!isSplit(a.last!), "final instruction cannot be a split point")
    var parts: [SplitPart] = []
    var remaining = a.dropFirst(0)
    while let splitPoint = remaining.first(where: isSplit) {
      let (splitIndex, tailEnd) = splitIndices(in: remaining, splittingAt: splitPoint)
      parts.append(
        SplitPart(
          before: Array(remaining.prefix(upTo: splitIndex)),
          splitPoint: splitPoint,
          tail: Array(remaining.suffix(from: splitIndex).dropFirst().prefix(upTo: tailEnd))))
      remaining = remaining.suffix(from: tailEnd)
    }
    // Add the last part.
    parts.append(SplitPart(before: Array(remaining), splitPoint: nil, tail: []))
    return parts
  }

  /// Splits `a` at the instruction `splitPoint`, returning the three parts (before, tail, after).
  ///
  /// - Precondition: `splitPoint` is contained in `a`.
  /// - Precondition: there is at least one instruction after `splitPoint` in `a`.
  /// - Postcondition: `r.before + [splitPoint] + r.tail + r.after == a`
  internal func split(instructions a: [InstructionID], at splitPoint: InstructionID) -> SplitTriplet {
    let (splitIndex, tailEnd) = splitIndices(in: a.dropFirst(0), splittingAt: splitPoint)
    return SplitTriplet(
      before: Array(a.prefix(upTo: splitIndex)),
      tail: Array(a.suffix(from: splitIndex).dropFirst().prefix(upTo: tailEnd)),
      after: Array(a.suffix(from: tailEnd))
    )
  }

  /// Splits the block `b` at the instruction `splitPoint`, returning the three parts
  /// (before, tail, after).
  ///
  /// - Precondition: `splitPoint` is an instruction in block `b`.
  /// - Precondition: there is at least one instruction after `splitPoint` in `b` (the block terminator).
  /// - Postcondition: `r.before + [splitPoint] + r.tail + r.after == instructions(in: b)`
  internal func split(block b: Block.ID, at splitPoint: InstructionID) -> SplitTriplet {
    split(instructions: Array(instructions(in: b)), at: splitPoint)
  }

  /// Splits `a` instructions at `s`, returning the split index and tail end index.
  private func splitIndices(
    in a: ArraySlice<InstructionID>, splittingAt s: InstructionID
  ) -> (splitIndex: Int, tailEnd: Int) {
    let splitIndex = a.firstIndex(of: s)
    precondition(splitIndex != nil, "split point \(s) not found")
    let remaining = a.suffix(from: splitIndex!).dropFirst()
    let tailEnd = remaining.firstIndex(where: { !Self.mayBeTail(self[$0]) })

    // At least the block terminator must be outside the tail, so `tailEnd` is never `nil`.
    return (splitIndex!, tailEnd!)
  }

  /// Returns `true` if the instruction `i` can be part of the yield tail,
  /// e.g., it appears after a yield, but still belong to the ramp of the projection.
  ///
  /// Tail instructions: `MarkState` (uninitialized), `DeallocStack`, `EndAccess`.
  private static func mayBeTail(_ i: Instruction) -> Bool {
    if let x = i as? MarkState {
      return !x.initialized
    } else {
      return i is EndAccess || i is DeallocStack
    }
  }

}

/// One contiguous region produced when a block's instructions are split at one or more split
/// points.
///
/// A `SplitPart` groups together the instructions that logically precede the next split point as
/// well as the "tail" instructions that must stay with them even though they appear afterward in
/// the original block. Both `before` and `tail` preserve the original ordering. It also records
/// the `splitPoint` instruction at which the split occurs; this is `nil` for the last part.
internal struct SplitPart {

  /// The instructions before the split point.
  internal let before: [InstructionID]

  /// The instruction at which the split occurs; nil for the last part.
  internal let splitPoint: InstructionID?

  /// The instructions physically after the split point, but logically belonging to before.
  internal let tail: [InstructionID]

}

/// A three-way split of a block's instructions around a split point.
///
/// The parts are:
/// - `before`: the instructions before the split point.
/// - `tail`: the instructions physically after the split point, but logically belonging to before.
/// - `after`: the instructions after the split point.
internal struct SplitTriplet {

  /// The instructions before the split point.
  internal let before: [InstructionID]

  /// The instructions physically after the split point, but logically belonging to before.
  internal let tail: [InstructionID]

  /// The instructions after the split point.
  internal let after: [InstructionID]

}
