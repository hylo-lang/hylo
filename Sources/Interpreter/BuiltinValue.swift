import IR

/// The value of a `Builtin` type instance.
enum BuiltinValue {

  /// 1-bit builtin integer type.
  case i1(Bool)

  /// 8-bit builtin integer type.
  case i8(UInt8)

  /// 16-bit builtin integer type.
  case i16(UInt16)

  /// 32-bit builtin integer type.
  case i32(UInt32)

  /// 64-bit builtin integer type.
  case i64(UInt64)

  /// 128-bit builtin integer type.
  case i128(UInt128)

  // TODO: support floats, word, ptr, module.

  /// 1-bit integer value, if present.
  var i1: Bool? {
    switch self {
    case .i1(let x): x
    default: nil
    }
  }

  /// 8-bit integer value, if present.
  var i8: UInt8? {
    switch self {
    case .i8(let x): x
    default: nil
    }
  }

  /// 16-bit integer value, if present.
  var i16: UInt16? {
    switch self {
    case .i16(let x): x
    default: nil
    }
  }

  /// 32-bit integer value, if present.
  var i32: UInt32? {
    switch self {
    case .i32(let x): x
    default: nil
    }
  }

  /// 64-bit integer value, if present.
  var i64: UInt64? {
    switch self {
    case .i64(let x): x
    default: nil
    }
  }

  /// 128-bit integer value, if present.
  var i128: UInt128? {
    switch self {
    case .i128(let x): x
    default: nil
    }
  }

}

/// Methods to create builtin value from IR constants.
extension BuiltinValue {

  /// An instance with value `c`.
  public init(_ c: IntegerConstant) {
    self =
      switch c.value.bitWidth {
      case 1: .i1(Bool(c.value != 0))
      case 8: .i8(UInt8(c.value))
      case 16: .i16(UInt16(c.value))
      case 32: .i32(UInt32(c.value))
      case 64: .i64(UInt64(c.value))
      case 128: .i128(UInt128(c.value))
      default: fatalError("Unknown bitwidth of integer \(c.value.bitWidth)")
      }
  }

}
