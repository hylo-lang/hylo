/// An instruction operand.
public enum Operand: Hashable {

  case inst(InstID)

  case parameter(block: Block.ID, index: Int)

  case constant(Constant)

  /// The ID of the function in which the operand is defined, if any.
  var function: Function.ID? {
    block?.function
  }

  /// The ID of the block in which the operand is defined, if any.
  var block: Block.ID? {
    switch self {
    case .inst(let instID):
      return Block.ID(function: instID.function, address: instID.block)
    case .parameter(let block, _):
      return block
    case .constant(_):
      return nil
    }
  }

  /// The payload of `.inst`, or `nil` if `self` is another case.
  var instID: InstID? {
    if case .inst(let id) = self {
      return id
    } else {
      return nil
    }
  }

}
