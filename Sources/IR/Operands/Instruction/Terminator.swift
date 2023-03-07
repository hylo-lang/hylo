/// An instruction that causes control flow to transfer.
protocol Terminator: Instruction {

  /// The basic blocks to which control flow may transfer.
  var successors: [Block.ID] { get }

  /// Replaces `old` by `new` and returns `true` if `old` is in `self.sucessors`. Otherwise,
  /// returns `false`.
  ///
  /// - Requires: `new` takes the same parameters as `old`.
  @discardableResult
  mutating func replaceSuccessor(_ old: Block.ID, _ new: Block.ID) -> Bool

}
