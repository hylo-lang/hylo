import LLVM

/// The name of an native instruction mapped to a built-in function.
///
/// Native instructions implement basis operations on built-in types, such as `Builtin.i64`, with
/// the same semantics as their corresponding LLVM instruction.
///
/// LLVM instructions are "generic" in the sense that they can be parameterized by types and flags.
/// For example, `add i32` and `add i64` represent the integer addition parameterized for 32-bit
/// and 64-bit integer values, respectively. In Val, this parameterization is encoded directly into
/// the name of a built-in function. For example, `Builtin.add_i64` corresponds to LLVM's `add i64`
/// instruction.
///
/// The name of a native instruction is given by the name of the corresponding LLVM instruction
/// concatenated with all its generic parameters, separated by underscores. For example:
///
///     add i64 -> Builtin.add_i64
///     icmp ne i32 -> Builtin.icmp_ne_i32
///     fmul fast double -> Builtin.fmul_fast_double
///
/// An exception is made for LLVM conversion instructions: we omit the keyword `to` that appears
/// between the first argument and result type. For example:
///
///     trunc i64 ... to i32 -> Builtin.trunc_i64_i32
///
/// Supported operations include all LLVM arithmetic and comparison instructions on built-in
/// integral and floating-point numbers as well as conversions from and to these types.
public enum NativeInstruction: Hashable {

  case add(LLVM.OverflowBehavior, BuiltinType)

  case sub(LLVM.OverflowBehavior, BuiltinType)

  case mul(LLVM.OverflowBehavior, BuiltinType)

  case shl(LLVM.OverflowBehavior, BuiltinType)

  case udiv(exact: Bool, BuiltinType)

  case sdiv(exact: Bool, BuiltinType)

  case lshr(exact: Bool, BuiltinType)

  case ashr(exact: Bool, BuiltinType)

  case urem(BuiltinType)

  case srem(BuiltinType)

  case and(BuiltinType)

  case or(BuiltinType)

  case xor(BuiltinType)

  case icmp(LLVM.IntegerPredicate, BuiltinType)

  case trunc(BuiltinType, BuiltinType)

  case zext(BuiltinType, BuiltinType)

  case sext(BuiltinType, BuiltinType)

  case uitofp(BuiltinType, BuiltinType)

  case sitofp(BuiltinType, BuiltinType)

  case fadd(MathFlags, BuiltinType)

  case fsub(MathFlags, BuiltinType)

  case fmul(MathFlags, BuiltinType)

  case fdiv(MathFlags, BuiltinType)

  case frem(MathFlags, BuiltinType)

  case fcmp(MathFlags, LLVM.FloatingPointPredicate, BuiltinType)

  case fptrunc(BuiltinType, BuiltinType)

  case fpext(BuiltinType, BuiltinType)

  case fptoui(BuiltinType, BuiltinType)

  case fptosi(BuiltinType, BuiltinType)

  case zeroinitializer(BuiltinType)

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

  /// The return type of this instruction.
  public var output: BuiltinType {
    switch self {
    case .add(_, let o):
      return o
    case .sub(_, let o):
      return o
    case .mul(_, let o):
      return o
    case .shl(_, let o):
      return o
    case .udiv(_, let o):
      return o
    case .sdiv(_, let o):
      return o
    case .lshr(_, let o):
      return o
    case .ashr(_, let o):
      return o
    case .urem(let o):
      return o
    case .srem(let o):
      return o
    case .and(let o):
      return o
    case .or(let o):
      return o
    case .xor(let o):
      return o
    case .icmp:
      return .i(1)
    case .trunc(_, let o):
      return o
    case .zext(_, let o):
      return o
    case .sext(_, let o):
      return o
    case .uitofp(_, let o):
      return o
    case .sitofp(_, let o):
      return o
    case .fadd(_, let o):
      return o
    case .fsub(_, let o):
      return o
    case .fmul(_, let o):
      return o
    case .fdiv(_, let o):
      return o
    case .frem(_, let o):
      return o
    case .fcmp:
      return .i(1)
    case .fptrunc(_, let o):
      return o
    case .fpext(_, let o):
      return o
    case .fptoui(_, let o):
      return o
    case .fptosi(_, let o):
      return o
    case .zeroinitializer(let o):
      return o
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
    case .zeroinitializer(let t):
      return "zeroinitializer_\(t)"
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
