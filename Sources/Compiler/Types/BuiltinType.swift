/// A built-in type.
public enum BuiltinType: TypeProtocol, Hashable {

  /// A built-in integer.
  ///
  /// This type represents the target's integer types. A built-in integer may be of any bit width
  /// and does not specify signedness.
  case i(Int)

  /// The type of the built-in module.
  case module

  public var flags: TypeFlags { .isCanonical }

}

extension BuiltinType: CustomStringConvertible {

  public var description: String {
    switch self {
    case .i(let bitWidth):
      return "i\(bitWidth)"
    case .module:
      return "Builtin"
    }
  }

}

extension BuiltinType: LosslessStringConvertible {

  public init?(_ description: String) {
    switch description {
    case "Builtin":
      self = .module

    default:
      if description.starts(with: "i") {
        if let bitWidth = Int(description.dropFirst()) {
          precondition(bitWidth > 0, "invalid bit width")
          self = .i(bitWidth)
        } else {
          return nil
        }
      } else {
        return nil
      }
    }
  }

}
