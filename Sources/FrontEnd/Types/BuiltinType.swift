/// A built-in type.
public enum BuiltinType: TypeProtocol {

  /// A built-in integer type.
  ///
  /// This type represents the target's integer types. A built-in integer may be of any bit width
  /// multiple of 8 and does not specify signedness.
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

  /// C Equivalent types (size, alignment and signedness may vary per platform).
  case cNumeric(BuiltinCNumericType)

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

public enum BuiltinCNumericType : UInt8 {
  case cChar
  case cUChar
  case cSChar
  case cShort
  case cUShort
  case cInt
  case cUInt
  case cLong
  case cULong
  case cLongLong
  case cULongLong
  case cFloat
  case cDouble
  case cLongDouble

  var isInteger: Bool {
    switch self {
    case .cChar, .cUChar, .cSChar, .cShort, .cUShort, .cInt, .cUInt, .cLong, .cULong, .cLongLong, .cULongLong:
      return true
    case .cFloat, .cDouble, .cLongDouble:
      return false
    }
  }
}

extension BuiltinType: CustomStringConvertible {
  /// The source identifier of `self`.
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
    case .cNumeric(let type):
      switch type {
        case .cChar:
        return "cchar"
      case .cUChar:
        return "cuchar"
      case .cSChar:
        return "cschar"
      case .cShort:
        return "cshort"
      case .cUShort:
        return "cushort"
      case .cInt:
        return "cint"
      case .cUInt:
        return "cuint"
      case .cLong:
        return "clong"
      case .cULong:
        return "culong"
      case .cLongLong:
        return "clonglong"
      case .cULongLong:
        return "culonglong"
      case .cFloat:
        return "cfloat"
      case .cDouble:   
        return "cdouble"
      case .cLongDouble:
        return "clongdouble"
      }
    case .module:
      return "Builtin"
    }
  }

}

extension BuiltinType: LosslessStringConvertible {

  /// Parses a source identifier into a `BuiltinType`.
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
    case "cchar":
      self = .cNumeric(.cChar)
    case "cuchar":
      self = .cNumeric(.cUChar)
    case "cschar":
      self = .cNumeric(.cSChar)
    case "cshort":
      self = .cNumeric(.cShort)
    case "cushort":  
      self = .cNumeric(.cUShort)
    case "cint":
      self = .cNumeric(.cInt)
    case "cuint":
      self = .cNumeric(.cUInt)
    case "clong":
      self = .cNumeric(.cLong)
    case "culong":
      self = .cNumeric(.cULong)
    case "clonglong":
      self = .cNumeric(.cLongLong)
    case "culonglong":
      self = .cNumeric(.cULongLong)
    case "cfloat":
      self = .cNumeric(.cFloat)
    case "cdouble":
      self = .cNumeric(.cDouble)
    case "clongdouble":
      self = .cNumeric(.cLongDouble)
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
