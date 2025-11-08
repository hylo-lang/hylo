import IR

/// The value of a `Builtin` type instance, stripped of type information.
struct UntypedBuiltinValue {

  /// The result of reinterpretation as an unsigned integer value.
  ///
  /// Equivalent to `UInt128(unsafeBitCast<T>(x))`, where `x` is the represented value
  /// and `T` is an unsigned integer of the same size as `x`.  Stores the unsigned
  /// representation of integer types.
  private let asUInt128: UInt128

  /// The size of the value in bytes.
  private let size: Int

  /// Creates instance of builtin value with unsigned reinterpretation `v` and size `s`.
  fileprivate init(asUInt128 v: UInt128, size s: Int) {
    asUInt128 = v
    size = s
  }

  /// Creates instance of builtin value with UInt8.
  public init(withUInt8 v: UInt8) {
    self.init(asUInt128: UInt128(v), size: 1)
  }

  /// Creates instance of builtin value with UInt16.
  public init(withUInt16 v: UInt16) {
    self.init(asUInt128: UInt128(v), size: 2)
  }

  /// Creates instance of builtin value with UInt32.
  public init(withUInt32 v: UInt32) {
    self.init(asUInt128: UInt128(v), size: 4)
  }

  /// Creates instance of builtin value with UInt64.
  public init(withUInt64 v: UInt64) {
    self.init(asUInt128: UInt128(v), size: 8)
  }

  /// Creates instance of builtin value with UInt128.
  public init(withUInt128 v: UInt128) {
    self.init(asUInt128: UInt128(v), size: 16)
  }

  /// Bool value, if present.
  public var bool: Bool? {
    if asUInt128 != 0 && asUInt128 != 1 { return nil }
    return asUInt128 != 0
  }

  /// UInt8 value, if present.
  public var i8: UInt8? { size == 1 ? UInt8(truncatingIfNeeded: asUInt128) : nil }

  /// UInt16 value, if present
  public var i16: UInt16? { size == 2 ? UInt16(truncatingIfNeeded: asUInt128) : nil }

  /// UInt32 value, if present
  public var i32: UInt32? { size == 4 ? UInt32(truncatingIfNeeded: asUInt128) : nil }

  /// UInt64 value, if present
  public var i64: UInt64? { size == 8 ? UInt64(truncatingIfNeeded: asUInt128) : nil }

  /// UInt128 value, if present
  public var i128: UInt128? { size == 16 ? asUInt128 : nil }

}

/// Methods to create builtin value from IR constants.
extension UntypedBuiltinValue {

  /// Creates instance of builtin value with integer constant `c`.
  public init(withIntegerConstant c: IntegerConstant) {
    self.init(asUInt128: UInt128(truncatingIfNeeded: c.value), size: (c.value.bitWidth + 7) / 8)
  }

}
