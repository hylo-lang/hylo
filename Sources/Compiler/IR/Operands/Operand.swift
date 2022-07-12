/// An instruction operand.
public enum Operand: Hashable {

  case inst(InstID)

  case parameter(block: Block.ID, index: Int)

  case constant(Constant)

  /// The payload of `.inst`, or `nil` if `self` is another case.
  var instID: InstID? {
    if case .inst(let id) = self {
      return id
    } else {
      return nil
    }
  }

}
