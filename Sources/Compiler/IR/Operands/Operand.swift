/// An instruction operand.
public enum Operand: Hashable {

  case inst(InstID)

  case parameter(block: Block.ID, index: Int)

  case constant(Constant)

  public func dump<Target: TextOutputStream>(
    into output: inout Target,
    with printer: inout IRPrinter
  ) {
    switch self {
    case .inst(let i):
      output.write(printer.translate(inst: i))
    case .parameter(_, let index):
      output.write("%\(index)")
    case .constant(let value):
      output.write(String(describing: value))
    }
  }

}
