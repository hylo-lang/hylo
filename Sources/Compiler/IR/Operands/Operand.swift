/// An instruction operand.
public enum Operand: Hashable {

  /// The `index`-th result of `inst`.
  case result(inst: InstID, index: Int)

  /// The `index`-th parameter of `block`.
  case parameter(block: Block.ID, index: Int)

  /// A constant value.
  case constant(Constant)

  /// The ID of the function in which the operand is defined, if any.
  var function: Function.ID? { block?.function }

  /// The ID of the block in which the operand is defined, if any.
  var block: Block.ID? {
    switch self {
    case .result(let inst, _): return Block.ID(function: inst.function, address: inst.block)
    case .parameter(let block, _): return block
    case .constant(_): return nil
    }
  }

  /// The ID of the instruction that produces this operand, if any.
  var inst: InstID? {
    switch self {
    case .result(let inst, _): return inst
    default: return nil
    }
  }

}
