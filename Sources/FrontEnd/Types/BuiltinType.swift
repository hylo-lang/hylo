/// A built-in type.
public enum BuiltinType: TypeProtocol {

  /// A built-in integer type.
  ///
  /// This type represents the target's integer types. A built-in integer may be of any bit width
  /// and does not specify signedness.
  case i(Int)

  /// An alias for `.i(n)` where `n` is the width of `.ptr`.
  case word

  /// A built-in 16-bit floating-point type (specifically, "binary16" in IEEE 754).
  case float16

  /// A built-in 32-bit floating-point type (specifically, "binary32" in IEEE 754).
  case float32

  /// A built-in 64-bit floating-point type (specifically, "binary64" in IEEE 754).
  case float64

  /// A built-in 128-bit floating-point type (specifically, "binary128" in IEEE 754).
  case float128

  /// A built-in opaque pointer.
  case ptr

  /// The type of the built-in module.
  case module

  /// The type of a union discriminator.
  public static let discriminator = word

  /// `true` iff `self` is `.i` or `.word`.
  public var isInteger: Bool {
    switch self {
    case .i, .word:
      return true
    default:
      return false
    }
  }

  public var flags: ValueFlags { .init() }

}

extension BuiltinType: CustomStringConvertible {

  public var description: String {
    switch self {
    case .i(let bitWidth):
      return "i\(bitWidth)"
    case .word:
      return "word"
    case .float16:
      return "float16"
    case .float32:
      return "float32"
    case .float64:
      return "float64"
    case .float128:
      return "float128"
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
    case "word":
      self = .word
    case "float16":
      self = .float16
    case "float32":
      self = .float32
    case "float64":
      self = .float64
    case "float128":
      self = .float128
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
