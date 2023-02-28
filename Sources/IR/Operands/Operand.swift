/// An instruction operand.
public enum Operand: Hashable {

  /// The `index`-th result of `instruction`.
  case result(instruction: InstructionID, index: Int)

  /// The `index`-th parameter of `block`.
  case parameter(block: Block.ID, index: Int)

  /// A constant value.
  case constant(Constant)

  // FIXME: this is just used for a precondition check.  Worth defining?
  /// The ID of the function in which the operand is defined, if any.
  var owner: Function.ID? {
    block?.owner
  }

  /// The ID of the block in which the operand is defined, if any.
  var block: Block.ID? {
    switch self {
    case .result(let instruction, _):
      return Block.ID(owner: instruction.function, address: instruction.block)
    case .parameter(let block, _):
      return block
    case .constant(_):
      return nil
    }
  }

  /// The ID of the instruction that produces this operand, if any.
  var instruction: InstructionID? {
    switch self {
    case .result(let instruction, _):
      return instruction
    default:
      return nil
    }
  }

}
