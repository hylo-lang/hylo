/// A built-in type.
public enum BuiltinType: TypeProtocol, Hashable {

  /// A built-in integer type.
  ///
  /// This type represents the target's integer types. A built-in integer may be of any bit width
  /// and does not specify signedness.
  case i(Int)

  /// A 64-bit floating point type (specifically, the "binary64" type defined in IEEE 754).
  case f64

  /// The type of the built-in module.
  case module

  public var flags: TypeFlags { .isCanonical }

}

extension BuiltinType: CustomStringConvertible {

  public var description: String {
    switch self {
    case .i(let bitWidth):
      return "i\(bitWidth)"
    case .f64:
      return "f64"
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
      }

      if description == "f64" {
        self = .f64
      }

      return nil
    }
  }

}
