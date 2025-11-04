import IR

/// Raw sclar value, stored as bits.
struct RawScalar {

  /// The result of reinterpretation as an unsigned integer value.
  ///
  /// Equivalent to `UInt128(unsafeBitCast<T>(x))`, where `x` is the represented value
  /// and `T` is an unsigned integer of the same size as `x`.  Stores the unsigned
  /// representation of integer types.
  fileprivate let asUInt128: UInt128

  /// The size of the value in bytes.
  fileprivate let size: Int

  /// Creates instance of builtin value with unsigned reinterpretation `v` and size `s`.
  fileprivate init(asUInt128 v: UInt128, size s: Int) {
    asUInt128 = v
    size = s
  }

}

/// Type representing builtin ptr.
enum Pointer {

  /// pointing to data in memory.
  case memory(Memory.Address)

  /// pointing to a function.
  case function(Function.ID)

  /// Returns address of object pointed by `self`.
  var address: Memory.Address? {
    switch self {
    case .memory(let addr): return addr
    case .function: return nil
    }
  }

  /// Identity of function pointed by `self`.
  var function: Function.ID? {
    switch self {
    case .memory: return nil
    case .function(let id): return id
    }
  }

}

/// The value of a `Builtin` type instance, stripped of type information.
enum UntypedBuiltinValue {

  /// Containing scalar value, stored inline.
  case scalar(RawScalar)

  /// Containing pointer.
  case pointer(Pointer)

  /// Creates instance of builtin value with UInt8.
  public init(withUInt8 v: UInt8) {
    self = .scalar(.init(asUInt128: UInt128(v), size: 1))
  }

  /// Creates instance of builtin value with UInt16.
  public init(withUInt16 v: UInt16) {
    self = .scalar(.init(asUInt128: UInt128(v), size: 2))
  }

  /// Creates instance of builtin value with UInt32.
  public init(withUInt32 v: UInt32) {
    self = .scalar(.init(asUInt128: UInt128(v), size: 4))
  }

  /// Creates instance of builtin value with UInt64.
  public init(withUInt64 v: UInt64) {
    self = .scalar(.init(asUInt128: UInt128(v), size: 8))
  }

  /// Creates instance of builtin value with UInt128.
  public init(withUInt128 v: UInt128) {
    self = .scalar(.init(asUInt128: UInt128(v), size: 16))
  }

  /// Creates instance of builtin ptr.
  public init(withPointer p: Pointer) {
    self = .pointer(p)
  }

  /// The scalar value
  private var scalar: RawScalar? {
    switch self {
    case .scalar(let x): return x
    case .pointer: return nil
    }
  }

  public var pointer: Pointer? {
    switch self {
    case .scalar: return nil
    case .pointer(let p): return p
    }
  }

  /// Bool value, if present.
  public var bool: Bool? {
    let s = scalar
    guard let s else { return nil }
    if s.asUInt128 != 0 && s.asUInt128 != 1 { return nil }
    return s.asUInt128 != 0
  }

  /// UInt8 value, if present.
  public var uint8: UInt8? {
    let s = scalar
    guard let s else { return nil }
    return s.size == 1 ? UInt8(truncatingIfNeeded: s.asUInt128) : nil
  }

  /// UInt16 value, if present
  public var uint16: UInt16? {
    let s = scalar
    guard let s else { return nil }
    return s.size == 2 ? UInt16(truncatingIfNeeded: s.asUInt128) : nil
  }

  /// UInt32 value, if present
  public var uint32: UInt32? {
    let s = scalar
    guard let s else { return nil }
    return s.size == 4 ? UInt32(truncatingIfNeeded: s.asUInt128) : nil
  }

  /// UInt64 value, if present
  public var uint64: UInt64? {
    let s = scalar
    guard let s else { return nil }
    return s.size == 8 ? UInt64(truncatingIfNeeded: s.asUInt128) : nil
  }

  /// UInt128 value, if present
  public var uint128: UInt128? {
    let s = scalar
    guard let s else { return nil }
    return s.size == 16 ? s.asUInt128 : nil
  }

}

/// Methods to create builtin value from IR constants.
extension UntypedBuiltinValue {

  /// Creates instance of builtin value with integer constant `c`.
  public init(withIntegerConstant c: IntegerConstant) {
    self = .scalar(
      .init(asUInt128: UInt128(truncatingIfNeeded: c.value), size: (c.value.bitWidth + 7) / 8))
  }

}
