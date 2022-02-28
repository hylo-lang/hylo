/// A linear sequence of instructions.
///
/// A basic block is well-formed if it contains a list of non-terminating instructions followed by
/// a single terminator. A VIL function contains at least one basic block unless it is a reference
/// to a function declarated in another module.
///
/// - Note: function applications and `await` instructions are not terminators, because execution
///   resumes at the same place after they are evaluated.
public struct BasicBlock {

  /// The function that contains the instruction.
  public let parent: String

  /// The formal parameters of the block.
  public let params: [ArgValue]

  /// The indices of the instructions in the block.
  public var instructions: [InstIndex] = []

  init(parent: String, paramTypes: [VILType]) {
    self.parent = parent
    self.params = paramTypes.map(ArgValue.init(type:))
  }

}
