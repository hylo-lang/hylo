/// A value identifying an instruction result or a parameter in an IR function.
enum FunctionLocal: Hashable {

  /// The result of an instruction.
  case register(block: Function.Blocks.Address, address: Block.Instructions.Address, index: Int)

  /// A block parameter.
  case parameter(block: Function.Blocks.Address, index: Int)

  /// Given `operand` is an instruction ID or a parameter, creates the corresponding key.
  /// Otherwise, returns `nil`.
  init?(operand: Operand) {
    switch operand {
    case .result(let instruction, let index):
      self = .register(block: instruction.block, address: instruction.address, index: index)
    case .parameter(let block, let index):
      self = .parameter(block: block.address, index: index)
    case .constant:
      return nil
    }
  }

  /// Creates an instruction key from an instruction ID and a result index.
  init(_ instruction: InstructionID, _ index: Int) {
    self = .register(block: instruction.block, address: instruction.address, index: index)
  }

  /// Returns an operand corresponding to that key.
  func operand(in function: Function.ID) -> Operand {
    switch self {
    case .register(let b, let i, let k):
      return .result(instruction: InstructionID(function, b, i), index: k)
    case .parameter(let b, let k):
      return .parameter(block: Block.ID(function: function, address: b), index: k)
    }
  }

}

extension FunctionLocal: CustomStringConvertible {

  var description: String {
    switch self {
    case .register(let b, let a, let i):
      return "Register(\(b.rawValue), \(a.rawValue), \(i))"
    case .parameter(let b, let i):
      return "Parameter(\(b.rawValue), \(i))"
    }
  }

}
