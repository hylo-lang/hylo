/// An instruction operand.
public enum Operand: Hashable {

  /// The `index`-th result of `instruction`.
  case register(InstructionID, Int)

  /// The `index`-th parameter of `block`.
  case parameter(Block.ID, Int)

  /// A constant value.
  case constant(Constant)

  /// The ID of the function in which the operand is defined, if any.
  var function: Function.ID? {
    block?.function
  }

  /// The ID of the block in which the operand is defined, if any.
  var block: Block.ID? {
    switch self {
    case .register(let instruction, _):
      return Block.ID(instruction.function, instruction.block)
    case .parameter(let block, _):
      return block
    case .constant(_):
      return nil
    }
  }

  /// The ID of the instruction that produces this operand, if any.
  var instruction: InstructionID? {
    switch self {
    case .register(let instruction, _):
      return instruction
    default:
      return nil
    }
  }

}

extension Operand: CustomStringConvertible {

  public var description: String {
    switch self {
    case .register(let i, let k):
      return "%\(i)#\(k)"
    case .parameter(let b, let k):
      return "%\(b)#\(k)"
    case .constant(let c):
      return c.description
    }
  }

}
