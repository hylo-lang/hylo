/// An instruction operand.
public enum Operand: Hashable {

  case inst(InstID)

  case parameter(block: Block.ID, index: Int)

  case constant(Constant)

}
