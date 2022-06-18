/// An instruction operand.
public enum Operand: Hashable {

  case inst(InstID)

  case parameter(Int)

  case constant(Constant)

}
