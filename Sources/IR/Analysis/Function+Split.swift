/// The positions, including epilogue information, at which a sequence of instructions is split.
/// 
/// Physically, a split point divides an array of instructions into two parts:
///   - the instructions at indices `< splitPoint` -- the "front"
///   - the instructions at indices `> splitPoint` -- the physical "back"
/// 
/// There are also instructions that are logically part of the front, even though they
/// appear after the split point. These are the "epilogue" instructions for a split point.
/// The `epilogueEnd` index indicates where the epilogue ends and the back begins.
/// 
/// Thus, a split point defines three contiguous regions of instructions:
///   - the "front" instructions at indices `< splitPoint`
///   - the "epilogue" instructions at indices in range `splitPoint ..< epilogueEnd`
///   - the "back" instructions at indices `>= epilogueEnd`
/// 
/// Invariant: `splitPoint < epilogueEnd`
internal struct SplitPosition {

  /// The index of the instruction at which the split occurs.
  internal let splitPoint: Int

  /// The index of the first instruction that is not part of the epilogue.
  internal let epilogueEnd: Int
}

extension Function {

  /// Splits `a`, instructions pointing into `self`, at all the points where `isSplitPoint` returns
  /// `true`, returning the splits.
  ///
  /// This is different from `Array.split(separator:)` as it deals with "epilogues". An epilogue is
  /// a contiguous sequence of instructions that appear immediately after a split point but
  /// logically belong before the split point.
  ///
  /// See also `SplitPosition`.
  internal func split(
    instructions a: [InstructionID], where isSplitPoint: (InstructionID) -> Bool
  ) -> [SplitPosition] {
    var r: [SplitPosition] = []
    var remaining = a.dropFirst(0)
    while let splitPointIndex = remaining.firstIndex(where: isSplitPoint) {
      remaining = a.suffix(from: splitPointIndex).dropFirst()
      let epilogueEnd = remaining.firstIndex(where: { !Self.mayBeEpilogue(self[$0]) }) ?? a.count
      r.append(SplitPosition(splitPoint: splitPointIndex, epilogueEnd: epilogueEnd))

      remaining = remaining.suffix(from: epilogueEnd)
    }
    return r
  }

  /// Splits `a`, instructions pointing into `self`, at `splitPoint`, returning the split position.
  ///
  /// This deals with the epilogue of `splitPoint`.
  ///
  /// See also `SplitPosition`.
  internal func split(
    instructions a: [InstructionID], at splitPoint: InstructionID
  ) -> SplitPosition {
    let splitPointIndex = a.firstIndex(of: splitPoint)
    precondition(splitPointIndex != nil, "split point \(splitPoint) not found")
    let remaining = a.suffix(from: splitPointIndex!).dropFirst()
    let epilogueEnd = remaining.firstIndex(where: { !Self.mayBeEpilogue(self[$0]) }) ?? a.count
    return SplitPosition(splitPoint: splitPointIndex!, epilogueEnd: epilogueEnd)
  }

  /// Splits instructions in `b` at `splitPoint`, returning the positions of the resulting partitions.
  ///
  /// This deals with the epilogue of `splitPoint`.
  ///
  /// See also `SplitPosition`.
  internal func split(block b: Block.ID, at splitPoint: InstructionID) -> SplitPosition {
    split(instructions: Array(instructions(in: b)), at: splitPoint)
  }

  /// Returns `true` if the instruction `i` can be part of the yield tail,
  /// e.g., it appears after a yield, but still belong to the ramp of the projection.
  ///
  /// Tail instructions: `MarkState` (uninitialized), `DeallocStack`, `EndAccess`.
  private static func mayBeEpilogue(_ i: Instruction) -> Bool {
    if let x = i as? MarkState {
      return !x.initialized
    } else {
      return i is EndAccess || i is DeallocStack
    }
  }

}
