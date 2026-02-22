/// The positions at which a sequence of instructions is split.
///
/// An instance defines three contiguous regions of instructions:
///   - the "front", at positions in the range `..<splitPoint`
///   - the "epilogue", at positions in the range `splitPoint ..< epilogueEnd`
///   - the "back", at positions in the range `epilogueEnd...`
///
/// If we strictly consider the ordering of instructions, a split point divides an array of
/// instructions into two parts:
///   - the instructions at indices `< splitPoint` -- the "front"
///   - the instructions at indices `> splitPoint` -- the "back"
///
/// There are also instructions that are logically part of the front, even though they
/// appear after the split point. These are the "epilogue" instructions for a split point.
/// The `epilogueEnd` index indicates where the epilogue ends and the back begins.
///
/// Invariant: `splitPoint < epilogueEnd`
internal struct SplitPositions {

  /// The index of the instruction at which the split occurs.
  internal let splitPoint: Int

  /// The index of the first instruction that is not part of the epilogue.
  internal let epilogueEnd: Int
}

extension Function {

  /// Splits `a`, which is a contiguous sequence of instructions in `self`, at all the points where `isSplitPoint` returns
  /// `true`, returning the splits.
  ///
  /// This method is different from `Array.split(separator:)` as it deals with "epilogues". An epilogue is
  /// a contiguous sequence of instructions that appears immediately after a split point but
  /// logically belongs before the split point.
  ///
  /// See also `SplitPositions`.
  internal func split(
    instructions xs: [InstructionID], where isSplitPoint: (InstructionID) -> Bool
  ) -> [SplitPositions] {
    var splits: [SplitPositions] = []
    var end = 0
    while let p = xs[end...].firstIndex(where: isSplitPoint) {
      end = xs[(p + 1)...].firstIndex(where: { !self[$0].mayBeEpilogue }) ?? xs.count
      splits.append(SplitPositions(splitPoint: p, epilogueEnd: end))
    }
    return splits
  }

  /// Splits `a`, instructions pointing into `self`, at `splitPoint`, returning the split position.
  ///
  /// This method deals with the epilogue of `splitPoint`.
  ///
  /// See also `SplitPositions`.
  internal func split(
    instructions a: [InstructionID], at splitPoint: InstructionID
  ) -> SplitPositions {
    let p = a.firstIndex(of: splitPoint)
    precondition(p != nil, "split point \(splitPoint) not found")
    let end = a[(p! + 1)...].firstIndex(where: { !self[$0].mayBeEpilogue }) ?? a.count
    return SplitPositions(splitPoint: p!, epilogueEnd: end)
  }

  /// Splits instructions in `b` at `splitPoint`, returning the positions of the resulting partitions.
  ///
  /// This method deals with the epilogue of `splitPoint`.
  ///
  /// See also `SplitPositions`.
  internal func split(block b: Block.ID, at splitPoint: InstructionID) -> SplitPositions {
    split(instructions: Array(instructions(in: b)), at: splitPoint)
  }

}

extension Instruction {

  /// Returns `true` if the instruction `i` can be part of the yield tail,
  /// e.g., it appears after a yield, but still belongs to the ramp of the projection.
  ///
  /// Tail instructions: `MarkState` (uninitialized), `DeallocStack`, `EndAccess`.
  var mayBeEpilogue: Bool {
    (self as? MarkState)?.initialized == false || (self is EndAccess) || (self is DeallocStack)
  }

}
