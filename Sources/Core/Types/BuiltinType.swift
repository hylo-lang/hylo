/// A built-in type.
public enum BuiltinType: TypeProtocol {

  /// A built-in integer type.
  ///
  /// This type represents the target's integer types. A built-in integer may be of any bit width
  /// and does not specify signedness.
  case i(Int)

  /// A built-in 16-bit floating-point type (specifically, "binary16" in IEEE 754).
  case half

  /// A built-in 32-bit floating-point type (specifically, "binary32" in IEEE 754).
  case float

  /// A built-in 64-bit floating-point type (specifically, "binary64" in IEEE 754).
  case double

  /// A built-in 128-bit floating-point type (specifically, "binary128" in IEEE 754).
  case fp128

  /// A built-in opaque pointer.
  case ptr

  /// The type of the built-in module.
  case module

  public var flags: TypeFlags { .isCanonical }

}

extension BuiltinType: CustomStringConvertible {

  public var description: String {
    switch self {
    case .i(let bitWidth):
      return "i\(bitWidth)"
    case .half:
      return "half"
    case .float:
      return "float"
    case .double:
      return "double"
    case .fp128:
      return "fp128"
    case .ptr:
      return "ptr"
    case .module:
      return "Builtin"
    }
  }

}

extension BuiltinType: LosslessStringConvertible {

  public init?<S: StringProtocol>(_ description: S) {
    switch description {
    case "half":
      self = .half
    case "float":
      self = .float
    case "double":
      self = .double
    case "fp128":
      self = .fp128
    case "ptr":
      self = .ptr
    case "Builtin":
      self = .module

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
