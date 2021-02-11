/// A basic instruction block.
///
/// A basic block is a single unit of control flow. It is a sequence of instructions, ended by a
/// "terminator" which indicates which block(s) should be executed next.
public final class BasicBlock {

  /// The function in which this block resides.
  public unowned let function: Function

  /// The arguments of the block.
  public var arguments: [Value]

  /// The instructions in the block.
  public var instructions: [Inst] = []

  init(function: Function, arguments: [Value]) {
    self.function = function
    self.arguments = arguments
  }

}
