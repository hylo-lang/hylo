/// A value identifying an instruction result or a parameter in an IR function.
enum FunctionLocal: Hashable {

  /// The result of an instruction.
  case result(block: Function.BlockAddress, address: Block.InstAddress, index: Int)

  /// A block parameter.
  case param(block: Function.BlockAddress, index: Int)

  /// Given `operand` is an instruction ID or a parameter, creates the corresponding key.
  /// Otherwise, returns `nil`.
  init?(operand: Operand) {
    switch operand {
    case .result(let inst, let index):
      self = .result(block: inst.block, address: inst.address, index: index)
    case .parameter(let block, let index): self = .param(block: block.address, index: index)
    case .constant: return nil
    }
  }

  /// Creates an instruction key from an instruction ID and a result index.
  init(_ inst: InstID, _ index: Int) {
    self = .result(block: inst.block, address: inst.address, index: index)
  }

  /// Returns an operand corresponding to that key.
  func operand(in function: Function.ID) -> Operand {
    switch self {
    case .result(let b, let i, let k):
      return .result(inst: InstID(function: function, block: b, address: i), index: k)
    case .param(let b, let k):
      return .parameter(block: Block.ID(function: function, address: b), index: k)
    }
  }

}
