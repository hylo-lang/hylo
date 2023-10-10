/// The name of an native instruction mapped to a built-in function.
///
/// Native instructions implement basis operations on built-in types, such as `Builtin.i64`, with
/// the same semantics as their corresponding LLVM instruction.
///
/// LLVM instructions are "generic" in the sense that they can be parameterized by types and flags.
/// For example, `add i32` and `add i64` represent the integer addition parameterized for 32-bit
/// and 64-bit integer values, respectively. In Hylo, this parameterization is encoded directly
/// into the name of a built-in function. For example, `Builtin.add_i64` corresponds to LLVM's
/// `add i64` instruction.
///
/// The name of a native instruction is given by the name of the corresponding LLVM instruction
/// concatenated with all its generic parameters, separated by underscores. For example:
///
///     add i64 -> Builtin.add_i64
///     icmp ne i32 -> Builtin.icmp_ne_i32
///     fmul fast float64 -> Builtin.fmul_fast_float64
///
/// An exception is made for LLVM conversion instructions: we omit the keyword `to` that appears
/// between the first argument and result type. For example:
///
///     trunc i64 ... to i32 -> Builtin.trunc_i64_i32
///
/// Supported operations include all LLVM arithmetic and comparison instructions on built-in
/// integral and floating-point numbers as well as conversions from and to these types.
public enum NativeInstruction: Hashable {

  case add(OverflowBehavior, BuiltinType)

  case sub(OverflowBehavior, BuiltinType)

  case mul(OverflowBehavior, BuiltinType)

  case shl(OverflowBehavior, BuiltinType)

  case udiv(exact: Bool, BuiltinType)

  case sdiv(exact: Bool, BuiltinType)

  case lshr(exact: Bool, BuiltinType)

  case ashr(exact: Bool, BuiltinType)

  case urem(BuiltinType)

  case srem(BuiltinType)

  case and(BuiltinType)

  case or(BuiltinType)

  case xor(BuiltinType)

  // Corresponding LLVM instruction: sadd.with.overflow
  case signedAdditionWithOverflow(BuiltinType)

  // Corresponding LLVM instruction: uadd.with.overflow
  case unsignedAdditionWithOverflow(BuiltinType)

  // Corresponding LLVM instruction: ssub.with.overflow
  case signedSubtractionWithOverflow(BuiltinType)

  // Corresponding LLVM instruction: usub.with.overflow
  case unsignedSubtractionWithOverflow(BuiltinType)

  // Corresponding LLVM instruction: smul.with.overflow
  case signedMultiplicationWithOverflow(BuiltinType)

  // Corresponding LLVM instruction: umul.with.overflow
  case unsignedMultiplicationWithOverflow(BuiltinType)

  case icmp(IntegerPredicate, BuiltinType)

  case trunc(BuiltinType, BuiltinType)

  case zext(BuiltinType, BuiltinType)

  case sext(BuiltinType, BuiltinType)

  case uitofp(BuiltinType, BuiltinType)

  case sitofp(BuiltinType, BuiltinType)

  case inttoptr(BuiltinType)

  case ptrtoint(BuiltinType)

  case fadd(MathFlags, BuiltinType)

  case fsub(MathFlags, BuiltinType)

  case fmul(MathFlags, BuiltinType)

  case fdiv(MathFlags, BuiltinType)

  case frem(MathFlags, BuiltinType)

  case fcmp(MathFlags, FloatingPointPredicate, BuiltinType)

  case fptrunc(BuiltinType, BuiltinType)

  case fpext(BuiltinType, BuiltinType)

  case fptoui(BuiltinType, BuiltinType)

  case fptosi(BuiltinType, BuiltinType)

  case ctpop(BuiltinType)

  case ctlz(BuiltinType)

  case cttz(BuiltinType)

  case zeroinitializer(BuiltinType)

  // Corresponding LLVM instruction: get_elementptr_inbounds.
  case advancedByBytes(byteOffset: BuiltinType)

  /// The parameters of a floating-point LLVM instruction.
  public struct MathFlags: OptionSet, Hashable {

    public typealias RawValue = UInt8

    public let rawValue: UInt8

    public init(rawValue: UInt8) {
      self.rawValue = rawValue
    }

    public static let afn = MathFlags(rawValue: 1 << 0)

    public static let arcp = MathFlags(rawValue: 1 << 1)

    public static let contract = MathFlags(rawValue: 1 << 2)

    public static let fast = MathFlags(rawValue: 1 << 3)

    public static let ninf = MathFlags(rawValue: 1 << 4)

    public static let nnan = MathFlags(rawValue: 1 << 5)

    public static let nsz = MathFlags(rawValue: 1 << 6)

    public static let reassoc = MathFlags(rawValue: 1 << 7)

  }

}

extension NativeInstruction {

  /// The type of this instruction when used as a function.
  public var type: LambdaType {
    switch self {
    case .add(_, let t):
      return .init(^t, ^t, to: ^t)
    case .sub(_, let t):
      return .init(^t, ^t, to: ^t)
    case .mul(_, let t):
      return .init(^t, ^t, to: ^t)
    case .shl(_, let t):
      return .init(^t, ^t, to: ^t)
    case .udiv(_, let t):
      return .init(^t, ^t, to: ^t)
    case .sdiv(_, let t):
      return .init(^t, ^t, to: ^t)
    case .lshr(_, let t):
      return .init(^t, ^t, to: ^t)
    case .ashr(_, let t):
      return .init(^t, ^t, to: ^t)
    case .urem(let t):
      return .init(^t, ^t, to: ^t)
    case .srem(let t):
      return .init(^t, ^t, to: ^t)
    case .and(let t):
      return .init(^t, ^t, to: ^t)
    case .or(let t):
      return .init(^t, ^t, to: ^t)
    case .xor(let t):
      return .init(^t, ^t, to: ^t)
    case .signedAdditionWithOverflow(let t):
      return .init(^t, ^t, to: ^TupleType(types: [^t, .builtin(.i(1))]))
    case .unsignedAdditionWithOverflow(let t):
      return .init(^t, ^t, to: ^TupleType(types: [^t, .builtin(.i(1))]))
    case .signedSubtractionWithOverflow(let t):
      return .init(^t, ^t, to: ^TupleType(types: [^t, .builtin(.i(1))]))
    case .unsignedSubtractionWithOverflow(let t):
      return .init(^t, ^t, to: ^TupleType(types: [^t, .builtin(.i(1))]))
    case .signedMultiplicationWithOverflow(let t):
      return .init(^t, ^t, to: ^TupleType(types: [^t, .builtin(.i(1))]))
    case .unsignedMultiplicationWithOverflow(let t):
      return .init(^t, ^t, to: ^TupleType(types: [^t, .builtin(.i(1))]))
    case .icmp(_, let t):
      return .init(^t, ^t, to: .builtin(.i(1)))
    case .trunc(let s, let d):
      return .init(^s, to: ^d)
    case .zext(let s, let d):
      return .init(^s, to: ^d)
    case .sext(let s, let d):
      return .init(^s, to: ^d)
    case .uitofp(let s, let d):
      return .init(^s, to: ^d)
    case .sitofp(let s, let d):
      return .init(^s, to: ^d)
    case .inttoptr(let t):
      return .init(^t, to: .builtin(.ptr))
    case .ptrtoint(let t):
      return .init(.builtin(.ptr), to: ^t)
    case .fadd(_, let t):
      return .init(^t, ^t, to: ^t)
    case .fsub(_, let t):
      return .init(^t, ^t, to: ^t)
    case .fmul(_, let t):
      return .init(^t, ^t, to: ^t)
    case .fdiv(_, let t):
      return .init(^t, ^t, to: ^t)
    case .frem(_, let t):
      return .init(^t, ^t, to: ^t)
    case .fcmp(_, _, let t):
      return .init(^t, ^t, to: .builtin(.i(1)))
    case .fptrunc(let s, let d):
      return .init(^s, to: ^d)
    case .fpext(let s, let d):
      return .init(^s, to: ^d)
    case .fptoui(let s, let d):
      return .init(^s, to: ^d)
    case .fptosi(let s, let d):
      return .init(^s, to: ^d)
    case .ctpop(let t):
      return .init(^t, to: ^t)
    case .ctlz(let t):
      return .init(^t, to: ^t)
    case .cttz(let t):
      return .init(^t, to: ^t)
    case .zeroinitializer(let t):
      return .init(to: ^t)
    case .advancedByBytes(let byteOffset):
      return .init(.builtin(.ptr), ^byteOffset, to: .builtin(.ptr))
    }
  }

}

extension NativeInstruction: CustomStringConvertible {

  public var description: String {
    switch self {
    case .add(let p, let t):
      return (p != .ignore) ? "add_\(p)_\(t)" : "add_\(t)"
    case .sub(let p, let t):
      return (p != .ignore) ? "sub_\(p)_\(t)" : "sub_\(t)"
    case .mul(let p, let t):
      return (p != .ignore) ? "mul_\(p)_\(t)" : "mul_\(t)"
    case .shl(let p, let t):
      return (p != .ignore) ? "shl_\(p)_\(t)" : "shl_\(t)"
    case .udiv(let e, let t):
      return e ? "udiv_exact_\(t)" : "udiv_\(t)"
    case .sdiv(let e, let t):
      return e ? "sdiv_exact_\(t)" : "sdiv_\(t)"
    case .lshr(let e, let t):
      return e ? "lshr_exact_\(t)" : "lshr_\(t)"
    case .ashr(let e, let t):
      return e ? "ashr_exact_\(t)" : "ashr_\(t)"
    case .urem(let t):
      return "urem_\(t)"
    case .srem(let t):
      return "srem_\(t)"
    case .and(let t):
      return "and_\(t)"
    case .or(let t):
      return "or_\(t)"
    case .xor(let t):
      return "xor_\(t)"
    case .signedAdditionWithOverflow(let t):
      return "sadd_with_overflow_\(t)"
    case .unsignedAdditionWithOverflow(let t):
      return "uadd_with_overflow_\(t)"
    case .signedSubtractionWithOverflow(let t):
      return "ssub_with_overflow_\(t)"
    case .unsignedSubtractionWithOverflow(let t):
      return "usub_with_overflow_\(t)"
    case .signedMultiplicationWithOverflow(let t):
      return "smul_with_overflow_\(t)"
    case .unsignedMultiplicationWithOverflow(let t):
      return "umul_with_overflow_\(t)"
    case .icmp(let p, let t):
      return "icmp_\(p)_\(t)"
    case .trunc(let l, let r):
      return "trunc_\(l)_\(r)"
    case .zext(let l, let r):
      return "zext_\(l)_\(r)"
    case .sext(let l, let r):
      return "sext_\(l)_\(r)"
    case .uitofp(let l, let r):
      return "uitofp_\(l)_\(r)"
    case .sitofp(let l, let r):
      return "sitofp_\(l)_\(r)"
    case .inttoptr(let t):
      return "inttoptr_\(t)"
    case .ptrtoint(let t):
      return "ptrtoint_\(t)"
    case .fadd(let f, let t):
      return f.isEmpty ? "fadd_\(t)" : "fadd_\(f)_\(t)"
    case .fsub(let f, let t):
      return f.isEmpty ? "fsub_\(t)" : "fsub_\(f)_\(t)"
    case .fmul(let f, let t):
      return f.isEmpty ? "fmul_\(t)" : "fmul_\(f)_\(t)"
    case .fdiv(let f, let t):
      return f.isEmpty ? "fdiv_\(t)" : "fdiv_\(f)_\(t)"
    case .frem(let f, let t):
      return f.isEmpty ? "frem_\(t)" : "frem_\(f)_\(t)"
    case .fcmp(let f, let p, let t):
      return f.isEmpty ? "fcmp_\(p)_\(t)" : "fcmp_\(f)_\(p)_\(t)"
    case .fptrunc(let l, let r):
      return "fptrunc_\(l)_\(r)"
    case .fpext(let l, let r):
      return "fpext_\(l)_\(r)"
    case .fptoui(let l, let r):
      return "fptoui_\(l)_\(r)"
    case .fptosi(let l, let r):
      return "fptosi_\(l)_\(r)"
    case .ctpop(let t):
      return "ctpop_\(t)"
    case .ctlz(let t):
      return "ctlz_\(t)"
    case .cttz(let t):
      return "cttz_\(t)"
    case .zeroinitializer(let t):
      return "zeroinitializer_\(t)"
    case .advancedByBytes(let t):
      return "advanced_by_bytes_\(t)"
    }
  }

}

extension NativeInstruction.MathFlags: CustomStringConvertible {

  public var description: String {
    var result: [String] = []
    if self.contains(.afn) { result.append("afn") }
    if self.contains(.arcp) { result.append("arcp") }
    if self.contains(.contract) { result.append("contract") }
    if self.contains(.fast) { result.append("fast") }
    if self.contains(.ninf) { result.append("ninf") }
    if self.contains(.nnan) { result.append("nnan") }
    if self.contains(.nsz) { result.append("nsz") }
    if self.contains(.reassoc) { result.append("reassoc") }
    return result.joined(separator: "_")
  }

}
