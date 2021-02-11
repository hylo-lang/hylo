import AST

/// A Val type that has been lowered to its VIL representation.
public enum VILType {

  case object(ValType)

  case address(ValType)

  public var isAddress: Bool {
    switch self {
    case .address: return true
    case .object : return false
    }
  }

}

extension VILType: CustomStringConvertible {

  public var description: String {
    switch self {
    case .object(let type):
      return String(describing: type)
    case .address(let type):
      return "*(\(type))"
    }
  }

}
