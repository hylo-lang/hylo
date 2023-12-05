/// An instruction that causes control flow to transfer.
protocol Terminator: Instruction {

  /// The basic blocks to which control flow may transfer.
  var successors: [Block.ID] { get }

  /// Replaces `old` with `new` and returns `true` if `old` is successor of `self`; returns `false`
  /// otherwise.
  ///
  /// - Requires: `new` takes the same parameters as `old`.
  @discardableResult
  mutating func replaceSuccessor(_ old: Block.ID, with new: Block.ID) -> Bool

}
