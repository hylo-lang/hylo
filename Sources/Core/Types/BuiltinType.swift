/// A built-in type.
public enum BuiltinType: TypeProtocol {

  /// A built-in integer type.
  ///
  /// This type represents the target's integer types. A built-in integer may be of any bit width
  /// and does not specify signedness.
  case i(Int)

  /// A built-in 64-bit floating-point type (specifically, "binary64" in IEEE 754).
  case double

  /// A built-in raw pointer pointer.
  case pointer

  /// The type of the built-in module.
  case module

  public var flags: TypeFlags { .isCanonical }

}

extension BuiltinType: CustomStringConvertible {

  public var description: String {
    switch self {
    case .i(let bitWidth):
      return "i\(bitWidth)"
    case .double:
      return "double"
    case .pointer:
      return "Pointer"
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
    case "double":
      self = .double
    case "Pointer":
      self = .pointer

    case _ where description.starts(with: "i"):
      if let bitWidth = Int(description.dropFirst()) {
        precondition(bitWidth > 0, "invalid bit width")
        self = .i(bitWidth)
      } else {
        return nil
      }

    default:
      return nil
    }
  }

}
