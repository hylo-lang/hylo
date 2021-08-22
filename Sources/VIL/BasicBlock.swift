/// A basic instruction block.
///
/// A basic block is a single unit of control flow. It is a sequence of instructions, ended by a
/// "terminator" which indicates which block(s) should be executed next.
public struct BasicBlock {

  public typealias ID = Int

  /// The arguments of the block.
  public var arguments: [Value]

  /// The instructions in the block.
  public var instructions: [Inst] = []

  init(arguments: [Value]) {
    self.arguments = arguments
  }

}
