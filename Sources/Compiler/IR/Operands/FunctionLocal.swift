/// A value identifying an instruction result or a parameter in an IR function.
enum FunctionLocal: Hashable {

  /// The result of an instruction.
  case result(block: Function.Blocks.Address, address: Block.Instructions.Address, index: Int)

  /// A block parameter.
  case param(block: Function.Blocks.Address, index: Int)

  /// Given `operand` is an instruction ID or a parameter, creates the corresponding key.
  /// Otherwise, returns `nil`.
  init?(operand: Operand) {
    switch operand {
    case .result(let instruction, let index):
      self = .result(block: instruction.block, address: instruction.address, index: index)
    case .parameter(let block, let index):
      self = .param(block: block.address, index: index)
    case .constant:
      return nil
    }
  }

  /// Creates an instruction key from an instruction ID and a result index.
  init(_ instruction: InstructionID, _ index: Int) {
    self = .result(block: instruction.block, address: instruction.address, index: index)
  }

  /// Returns an operand corresponding to that key.
  func operand(in function: Function.ID) -> Operand {
    switch self {
    case .result(let b, let i, let k):
      return .result(instruction: InstructionID(function: function, block: b, address: i), index: k)
    case .param(let b, let k):
      return .parameter(block: Block.ID(function: function, address: b), index: k)
    }
  }

}
