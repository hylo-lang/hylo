import Utils

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
public enum BuiltinFunction: Hashable {

  /// `Builtin.address<T>(of v: T) -> Builtin.ptr`
  ///
  /// Returns a pointer to the storage of the argument.
  ///
  /// The resulting pointer is dereferenceable only for the lifetime of the argument; additional
  /// measures may be needed to keep the argument alive during the pointer's use.
  case addressOf

  /// `Builtin.mark_uninitialized<T>(_ v: sink T) -> Void`
  ///
  /// Marks `v` as being uninitialized.
  case markUninitialized

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

  case atomic_store_relaxed(BuiltinType)

  case atomic_store_release(BuiltinType)

  case atomic_store_seqcst(BuiltinType)

  case atomic_load_relaxed(BuiltinType)

  case atomic_load_acquire(BuiltinType)

  case atomic_load_seqcst(BuiltinType)

  case atomic_swap_relaxed(BuiltinType)

  case atomic_swap_acquire(BuiltinType)

  case atomic_swap_release(BuiltinType)

  case atomic_swap_acqrel(BuiltinType)

  case atomic_swap_seqcst(BuiltinType)

  case atomic_add_relaxed(BuiltinType)

  case atomic_add_acquire(BuiltinType)

  case atomic_add_release(BuiltinType)

  case atomic_add_acqrel(BuiltinType)

  case atomic_add_seqcst(BuiltinType)

  case atomic_fadd_relaxed(BuiltinType)

  case atomic_fadd_acquire(BuiltinType)

  case atomic_fadd_release(BuiltinType)

  case atomic_fadd_acqrel(BuiltinType)

  case atomic_fadd_seqcst(BuiltinType)

  case atomic_sub_relaxed(BuiltinType)

  case atomic_sub_acquire(BuiltinType)

  case atomic_sub_release(BuiltinType)

  case atomic_sub_acqrel(BuiltinType)

  case atomic_sub_seqcst(BuiltinType)

  case atomic_fsub_relaxed(BuiltinType)

  case atomic_fsub_acquire(BuiltinType)

  case atomic_fsub_release(BuiltinType)

  case atomic_fsub_acqrel(BuiltinType)

  case atomic_fsub_seqcst(BuiltinType)

  case atomic_max_relaxed(BuiltinType)

  case atomic_max_acquire(BuiltinType)

  case atomic_max_release(BuiltinType)

  case atomic_max_acqrel(BuiltinType)

  case atomic_max_seqcst(BuiltinType)

  case atomic_umax_relaxed(BuiltinType)

  case atomic_umax_acquire(BuiltinType)

  case atomic_umax_release(BuiltinType)

  case atomic_umax_acqrel(BuiltinType)

  case atomic_umax_seqcst(BuiltinType)

  case atomic_fmax_relaxed(BuiltinType)

  case atomic_fmax_acquire(BuiltinType)

  case atomic_fmax_release(BuiltinType)

  case atomic_fmax_acqrel(BuiltinType)

  case atomic_fmax_seqcst(BuiltinType)

  case atomic_min_relaxed(BuiltinType)

  case atomic_min_acquire(BuiltinType)

  case atomic_min_release(BuiltinType)

  case atomic_min_acqrel(BuiltinType)

  case atomic_min_seqcst(BuiltinType)

  case atomic_umin_relaxed(BuiltinType)

  case atomic_umin_acquire(BuiltinType)

  case atomic_umin_release(BuiltinType)

  case atomic_umin_acqrel(BuiltinType)

  case atomic_umin_seqcst(BuiltinType)

  case atomic_fmin_relaxed(BuiltinType)

  case atomic_fmin_acquire(BuiltinType)

  case atomic_fmin_release(BuiltinType)

  case atomic_fmin_acqrel(BuiltinType)

  case atomic_fmin_seqcst(BuiltinType)

  case atomic_and_relaxed(BuiltinType)

  case atomic_and_acquire(BuiltinType)

  case atomic_and_release(BuiltinType)

  case atomic_and_acqrel(BuiltinType)

  case atomic_and_seqcst(BuiltinType)

  case atomic_nand_relaxed(BuiltinType)

  case atomic_nand_acquire(BuiltinType)

  case atomic_nand_release(BuiltinType)

  case atomic_nand_acqrel(BuiltinType)

  case atomic_nand_seqcst(BuiltinType)

  case atomic_or_relaxed(BuiltinType)

  case atomic_or_acquire(BuiltinType)

  case atomic_or_release(BuiltinType)

  case atomic_or_acqrel(BuiltinType)

  case atomic_or_seqcst(BuiltinType)

  case atomic_xor_relaxed(BuiltinType)

  case atomic_xor_acquire(BuiltinType)

  case atomic_xor_release(BuiltinType)

  case atomic_xor_acqrel(BuiltinType)

  case atomic_xor_seqcst(BuiltinType)

  case atomic_cmpxchg_relaxed_relaxed(BuiltinType)

  case atomic_cmpxchg_relaxed_acquire(BuiltinType)

  case atomic_cmpxchg_relaxed_seqcst(BuiltinType)

  case atomic_cmpxchg_acquire_relaxed(BuiltinType)

  case atomic_cmpxchg_acquire_acquire(BuiltinType)

  case atomic_cmpxchg_acquire_seqcst(BuiltinType)

  case atomic_cmpxchg_release_relaxed(BuiltinType)

  case atomic_cmpxchg_release_acquire(BuiltinType)

  case atomic_cmpxchg_release_seqcst(BuiltinType)

  case atomic_cmpxchg_acqrel_relaxed(BuiltinType)

  case atomic_cmpxchg_acqrel_acquire(BuiltinType)

  case atomic_cmpxchg_acqrel_seqcst(BuiltinType)

  case atomic_cmpxchg_seqcst_relaxed(BuiltinType)

  case atomic_cmpxchg_seqcst_acquire(BuiltinType)

  case atomic_cmpxchg_seqcst_seqcst(BuiltinType)

  case atomic_cmpxchgweak_relaxed_relaxed(BuiltinType)

  case atomic_cmpxchgweak_relaxed_acquire(BuiltinType)

  case atomic_cmpxchgweak_relaxed_seqcst(BuiltinType)

  case atomic_cmpxchgweak_acquire_relaxed(BuiltinType)

  case atomic_cmpxchgweak_acquire_acquire(BuiltinType)

  case atomic_cmpxchgweak_acquire_seqcst(BuiltinType)

  case atomic_cmpxchgweak_release_relaxed(BuiltinType)

  case atomic_cmpxchgweak_release_acquire(BuiltinType)

  case atomic_cmpxchgweak_release_seqcst(BuiltinType)

  case atomic_cmpxchgweak_acqrel_relaxed(BuiltinType)

  case atomic_cmpxchgweak_acqrel_acquire(BuiltinType)

  case atomic_cmpxchgweak_acqrel_seqcst(BuiltinType)

  case atomic_cmpxchgweak_seqcst_relaxed(BuiltinType)

  case atomic_cmpxchgweak_seqcst_acquire(BuiltinType)

  case atomic_cmpxchgweak_seqcst_seqcst(BuiltinType)

  case atomic_fence_acquire

  case atomic_fence_release

  case atomic_fence_acqrel

  case atomic_fence_seqcst

  case atomic_singlethreadfence_acquire

  case atomic_singlethreadfence_release

  case atomic_singlethreadfence_acqrel

  case atomic_singlethreadfence_seqcst


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

extension BuiltinFunction {

  /// The function's result type.
  public var output: AnyType {
    switch self {
    case .addressOf: .builtin(.ptr)
    case .markUninitialized: .void
    default: type(makingFreshVariableWith: { fatalError("unreachable") }).output
    }
  }

  /// Returns the type of the function, calling `freshVariable` to create fresh type variables.
  public func type(makingFreshVariableWith freshVariable: () -> TypeVariable) -> ArrowType {
    switch self {
    case .addressOf:
      let p = ParameterType(.let, ^freshVariable())
      return .init(inputs: [.init(label: "of", type: ^p)], output: .builtin(.ptr))

    case .markUninitialized:
      let p = ParameterType(.let, ^freshVariable())
      return .init(inputs: [.init(label: nil, type: ^p)], output: .void)

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
    case .atomic_store_relaxed(let t):
      return .init(.builtin(.ptr), ^t, to: .void)
    case .atomic_store_release(let t):
      return .init(.builtin(.ptr), ^t, to: .void)
    case .atomic_store_seqcst(let t):
      return .init(.builtin(.ptr), ^t, to: .void)
    case .atomic_load_relaxed(let t):
      return .init(.builtin(.ptr), to: ^t)
    case .atomic_load_acquire(let t):
      return .init(.builtin(.ptr), to: ^t)
    case .atomic_load_seqcst(let t):
      return .init(.builtin(.ptr), to: ^t)
    case .atomic_swap_relaxed(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_swap_acquire(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_swap_release(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_swap_acqrel(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_swap_seqcst(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_add_relaxed(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_add_acquire(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_add_release(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_add_acqrel(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_add_seqcst(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_fadd_relaxed(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_fadd_acquire(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_fadd_release(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_fadd_acqrel(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_fadd_seqcst(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_sub_relaxed(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_sub_acquire(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_sub_release(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_sub_acqrel(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_sub_seqcst(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_fsub_relaxed(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_fsub_acquire(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_fsub_release(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_fsub_acqrel(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_fsub_seqcst(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_max_relaxed(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_max_acquire(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_max_release(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_max_acqrel(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_max_seqcst(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_umax_relaxed(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_umax_acquire(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_umax_release(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_umax_acqrel(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_umax_seqcst(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_fmax_relaxed(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_fmax_acquire(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_fmax_release(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_fmax_acqrel(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_fmax_seqcst(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_min_relaxed(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_min_acquire(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_min_release(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_min_acqrel(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_min_seqcst(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_umin_relaxed(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_umin_acquire(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_umin_release(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_umin_acqrel(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_umin_seqcst(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_fmin_relaxed(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_fmin_acquire(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_fmin_release(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_fmin_acqrel(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_fmin_seqcst(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_and_relaxed(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_and_acquire(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_and_release(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_and_acqrel(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_and_seqcst(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_nand_relaxed(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_nand_acquire(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_nand_release(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_nand_acqrel(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_nand_seqcst(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_or_relaxed(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_or_acquire(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_or_release(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_or_acqrel(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_or_seqcst(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_xor_relaxed(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_xor_acquire(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_xor_release(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_xor_acqrel(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_xor_seqcst(let t):
      return .init(.builtin(.ptr), ^t, to: ^t)
    case .atomic_cmpxchg_relaxed_relaxed(let t):
      return .init(.builtin(.ptr), ^t, ^t, to: ^TupleType(types: [^t, .builtin(.i(1))]))
    case .atomic_cmpxchg_relaxed_acquire(let t):
      return .init(.builtin(.ptr), ^t, ^t, to: ^TupleType(types: [^t, .builtin(.i(1))]))
    case .atomic_cmpxchg_relaxed_seqcst(let t):
      return .init(.builtin(.ptr), ^t, ^t, to: ^TupleType(types: [^t, .builtin(.i(1))]))
    case .atomic_cmpxchg_acquire_relaxed(let t):
      return .init(.builtin(.ptr), ^t, ^t, to: ^TupleType(types: [^t, .builtin(.i(1))]))
    case .atomic_cmpxchg_acquire_acquire(let t):
      return .init(.builtin(.ptr), ^t, ^t, to: ^TupleType(types: [^t, .builtin(.i(1))]))
    case .atomic_cmpxchg_acquire_seqcst(let t):
      return .init(.builtin(.ptr), ^t, ^t, to: ^TupleType(types: [^t, .builtin(.i(1))]))
    case .atomic_cmpxchg_release_relaxed(let t):
      return .init(.builtin(.ptr), ^t, ^t, to: ^TupleType(types: [^t, .builtin(.i(1))]))
    case .atomic_cmpxchg_release_acquire(let t):
      return .init(.builtin(.ptr), ^t, ^t, to: ^TupleType(types: [^t, .builtin(.i(1))]))
    case .atomic_cmpxchg_release_seqcst(let t):
      return .init(.builtin(.ptr), ^t, ^t, to: ^TupleType(types: [^t, .builtin(.i(1))]))
    case .atomic_cmpxchg_acqrel_relaxed(let t):
      return .init(.builtin(.ptr), ^t, ^t, to: ^TupleType(types: [^t, .builtin(.i(1))]))
    case .atomic_cmpxchg_acqrel_acquire(let t):
      return .init(.builtin(.ptr), ^t, ^t, to: ^TupleType(types: [^t, .builtin(.i(1))]))
    case .atomic_cmpxchg_acqrel_seqcst(let t):
      return .init(.builtin(.ptr), ^t, ^t, to: ^TupleType(types: [^t, .builtin(.i(1))]))
    case .atomic_cmpxchg_seqcst_relaxed(let t):
      return .init(.builtin(.ptr), ^t, ^t, to: ^TupleType(types: [^t, .builtin(.i(1))]))
    case .atomic_cmpxchg_seqcst_acquire(let t):
      return .init(.builtin(.ptr), ^t, ^t, to: ^TupleType(types: [^t, .builtin(.i(1))]))
    case .atomic_cmpxchg_seqcst_seqcst(let t):
      return .init(.builtin(.ptr), ^t, ^t, to: ^TupleType(types: [^t, .builtin(.i(1))]))
    case .atomic_cmpxchgweak_relaxed_relaxed(let t):
      return .init(.builtin(.ptr), ^t, ^t, to: ^TupleType(types: [^t, .builtin(.i(1))]))
    case .atomic_cmpxchgweak_relaxed_acquire(let t):
      return .init(.builtin(.ptr), ^t, ^t, to: ^TupleType(types: [^t, .builtin(.i(1))]))
    case .atomic_cmpxchgweak_relaxed_seqcst(let t):
      return .init(.builtin(.ptr), ^t, ^t, to: ^TupleType(types: [^t, .builtin(.i(1))]))
    case .atomic_cmpxchgweak_acquire_relaxed(let t):
      return .init(.builtin(.ptr), ^t, ^t, to: ^TupleType(types: [^t, .builtin(.i(1))]))
    case .atomic_cmpxchgweak_acquire_acquire(let t):
      return .init(.builtin(.ptr), ^t, ^t, to: ^TupleType(types: [^t, .builtin(.i(1))]))
    case .atomic_cmpxchgweak_acquire_seqcst(let t):
      return .init(.builtin(.ptr), ^t, ^t, to: ^TupleType(types: [^t, .builtin(.i(1))]))
    case .atomic_cmpxchgweak_release_relaxed(let t):
      return .init(.builtin(.ptr), ^t, ^t, to: ^TupleType(types: [^t, .builtin(.i(1))]))
    case .atomic_cmpxchgweak_release_acquire(let t):
      return .init(.builtin(.ptr), ^t, ^t, to: ^TupleType(types: [^t, .builtin(.i(1))]))
    case .atomic_cmpxchgweak_release_seqcst(let t):
      return .init(.builtin(.ptr), ^t, ^t, to: ^TupleType(types: [^t, .builtin(.i(1))]))
    case .atomic_cmpxchgweak_acqrel_relaxed(let t):
      return .init(.builtin(.ptr), ^t, ^t, to: ^TupleType(types: [^t, .builtin(.i(1))]))
    case .atomic_cmpxchgweak_acqrel_acquire(let t):
      return .init(.builtin(.ptr), ^t, ^t, to: ^TupleType(types: [^t, .builtin(.i(1))]))
    case .atomic_cmpxchgweak_acqrel_seqcst(let t):
      return .init(.builtin(.ptr), ^t, ^t, to: ^TupleType(types: [^t, .builtin(.i(1))]))
    case .atomic_cmpxchgweak_seqcst_relaxed(let t):
      return .init(.builtin(.ptr), ^t, ^t, to: ^TupleType(types: [^t, .builtin(.i(1))]))
    case .atomic_cmpxchgweak_seqcst_acquire(let t):
      return .init(.builtin(.ptr), ^t, ^t, to: ^TupleType(types: [^t, .builtin(.i(1))]))
    case .atomic_cmpxchgweak_seqcst_seqcst(let t):
      return .init(.builtin(.ptr), ^t, ^t, to: ^TupleType(types: [^t, .builtin(.i(1))]))
    case .atomic_fence_acquire:
      return .init(to: .void)
    case .atomic_fence_release:
      return .init(to: .void)
    case .atomic_fence_acqrel:
      return .init(to: .void)
    case .atomic_fence_seqcst:
      return .init(to: .void)
    case .atomic_singlethreadfence_acquire:
      return .init(to: .void)
    case .atomic_singlethreadfence_release:
      return .init(to: .void)
    case .atomic_singlethreadfence_acqrel:
      return .init(to: .void)
    case .atomic_singlethreadfence_seqcst:
      return .init(to: .void)
    }
  }

}

extension BuiltinFunction: CustomStringConvertible {

  public var description: String {
    switch self {
    case .addressOf:
      return "address(of:)"
    case .markUninitialized:
      return "mark_uninitialized(_:)"
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
    case .atomic_store_relaxed(let t):
      return "atomic_store_relaxed_\(t)"
    case .atomic_store_release(let t):
      return "atomic_store_release_\(t)"
    case .atomic_store_seqcst(let t):
      return "atomic_store_seqcst_\(t)"
    case .atomic_load_relaxed(let t):
      return "atomic_load_relaxed_\(t)"
    case .atomic_load_acquire(let t):
      return "atomic_load_acquire_\(t)"
    case .atomic_load_seqcst(let t):
      return "atomic_load_seqcst_\(t)"
    case .atomic_swap_relaxed(let t):
      return "atomic_swap_relaxed_\(t)"
    case .atomic_swap_acquire(let t):
      return "atomic_swap_acquire_\(t)"
    case .atomic_swap_release(let t):
      return "atomic_swap_release_\(t)"
    case .atomic_swap_acqrel(let t):
      return "atomic_swap_acqrel_\(t)"
    case .atomic_swap_seqcst(let t):
      return "atomic_swap_seqcst_\(t)"
    case .atomic_add_relaxed(let t):
      return "atomic_add_relaxed_\(t)"
    case .atomic_add_acquire(let t):
      return "atomic_add_acquire_\(t)"
    case .atomic_add_release(let t):
      return "atomic_add_release_\(t)"
    case .atomic_add_acqrel(let t):
      return "atomic_add_acqrel_\(t)"
    case .atomic_add_seqcst(let t):
      return "atomic_add_seqcst_\(t)"
    case .atomic_fadd_relaxed(let t):
      return "atomic_fadd_relaxed_\(t)"
    case .atomic_fadd_acquire(let t):
      return "atomic_fadd_acquire_\(t)"
    case .atomic_fadd_release(let t):
      return "atomic_fadd_release_\(t)"
    case .atomic_fadd_acqrel(let t):
      return "atomic_fadd_acqrel_\(t)"
    case .atomic_fadd_seqcst(let t):
      return "atomic_fadd_seqcst_\(t)"
    case .atomic_sub_relaxed(let t):
      return "atomic_sub_relaxed_\(t)"
    case .atomic_sub_acquire(let t):
      return "atomic_sub_acquire_\(t)"
    case .atomic_sub_release(let t):
      return "atomic_sub_release_\(t)"
    case .atomic_sub_acqrel(let t):
      return "atomic_sub_acqrel_\(t)"
    case .atomic_sub_seqcst(let t):
      return "atomic_sub_seqcst_\(t)"
    case .atomic_fsub_relaxed(let t):
      return "atomic_fsub_relaxed_\(t)"
    case .atomic_fsub_acquire(let t):
      return "atomic_fsub_acquire_\(t)"
    case .atomic_fsub_release(let t):
      return "atomic_fsub_release_\(t)"
    case .atomic_fsub_acqrel(let t):
      return "atomic_fsub_acqrel_\(t)"
    case .atomic_fsub_seqcst(let t):
      return "atomic_fsub_seqcst_\(t)"
    case .atomic_max_relaxed(let t):
      return "atomic_max_relaxed_\(t)"
    case .atomic_max_acquire(let t):
      return "atomic_max_acquire_\(t)"
    case .atomic_max_release(let t):
      return "atomic_max_release_\(t)"
    case .atomic_max_acqrel(let t):
      return "atomic_max_acqrel_\(t)"
    case .atomic_max_seqcst(let t):
      return "atomic_max_seqcst_\(t)"
    case .atomic_umax_relaxed(let t):
      return "atomic_umax_relaxed_\(t)"
    case .atomic_umax_acquire(let t):
      return "atomic_umax_acquire_\(t)"
    case .atomic_umax_release(let t):
      return "atomic_umax_release_\(t)"
    case .atomic_umax_acqrel(let t):
      return "atomic_umax_acqrel_\(t)"
    case .atomic_umax_seqcst(let t):
      return "atomic_umax_seqcst_\(t)"
    case .atomic_fmax_relaxed(let t):
      return "atomic_fmax_relaxed_\(t)"
    case .atomic_fmax_acquire(let t):
      return "atomic_fmax_acquire_\(t)"
    case .atomic_fmax_release(let t):
      return "atomic_fmax_release_\(t)"
    case .atomic_fmax_acqrel(let t):
      return "atomic_fmax_acqrel_\(t)"
    case .atomic_fmax_seqcst(let t):
      return "atomic_fmax_seqcst_\(t)"
    case .atomic_min_relaxed(let t):
      return "atomic_min_relaxed_\(t)"
    case .atomic_min_acquire(let t):
      return "atomic_min_acquire_\(t)"
    case .atomic_min_release(let t):
      return "atomic_min_release_\(t)"
    case .atomic_min_acqrel(let t):
      return "atomic_min_acqrel_\(t)"
    case .atomic_min_seqcst(let t):
      return "atomic_min_seqcst_\(t)"
    case .atomic_umin_relaxed(let t):
      return "atomic_umin_relaxed_\(t)"
    case .atomic_umin_acquire(let t):
      return "atomic_umin_acquire_\(t)"
    case .atomic_umin_release(let t):
      return "atomic_umin_release_\(t)"
    case .atomic_umin_acqrel(let t):
      return "atomic_umin_acqrel_\(t)"
    case .atomic_umin_seqcst(let t):
      return "atomic_umin_seqcst_\(t)"
    case .atomic_fmin_relaxed(let t):
      return "atomic_fmin_relaxed_\(t)"
    case .atomic_fmin_acquire(let t):
      return "atomic_fmin_acquire_\(t)"
    case .atomic_fmin_release(let t):
      return "atomic_fmin_release_\(t)"
    case .atomic_fmin_acqrel(let t):
      return "atomic_fmin_acqrel_\(t)"
    case .atomic_fmin_seqcst(let t):
      return "atomic_fmin_seqcst_\(t)"
    case .atomic_and_relaxed(let t):
      return "atomic_and_relaxed_\(t)"
    case .atomic_and_acquire(let t):
      return "atomic_and_acquire_\(t)"
    case .atomic_and_release(let t):
      return "atomic_and_release_\(t)"
    case .atomic_and_acqrel(let t):
      return "atomic_and_acqrel_\(t)"
    case .atomic_and_seqcst(let t):
      return "atomic_and_seqcst_\(t)"
    case .atomic_nand_relaxed(let t):
      return "atomic_nand_relaxed_\(t)"
    case .atomic_nand_acquire(let t):
      return "atomic_nand_acquire_\(t)"
    case .atomic_nand_release(let t):
      return "atomic_nand_release_\(t)"
    case .atomic_nand_acqrel(let t):
      return "atomic_nand_acqrel_\(t)"
    case .atomic_nand_seqcst(let t):
      return "atomic_nand_seqcst_\(t)"
    case .atomic_or_relaxed(let t):
      return "atomic_or_relaxed_\(t)"
    case .atomic_or_acquire(let t):
      return "atomic_or_acquire_\(t)"
    case .atomic_or_release(let t):
      return "atomic_or_release_\(t)"
    case .atomic_or_acqrel(let t):
      return "atomic_or_acqrel_\(t)"
    case .atomic_or_seqcst(let t):
      return "atomic_or_seqcst_\(t)"
    case .atomic_xor_relaxed(let t):
      return "atomic_xor_relaxed_\(t)"
    case .atomic_xor_acquire(let t):
      return "atomic_xor_acquire_\(t)"
    case .atomic_xor_release(let t):
      return "atomic_xor_release_\(t)"
    case .atomic_xor_acqrel(let t):
      return "atomic_xor_acqrel_\(t)"
    case .atomic_xor_seqcst(let t):
      return "atomic_xor_seqcst_\(t)"
    case .atomic_cmpxchg_relaxed_relaxed(let t):
      return "atomic_cmpxchg_relaxed_relaxed_\(t)"
    case .atomic_cmpxchg_relaxed_acquire(let t):
      return "atomic_cmpxchg_relaxed_acquire_\(t)"
    case .atomic_cmpxchg_relaxed_seqcst(let t):
      return "atomic_cmpxchg_relaxed_seqcst_\(t)"
    case .atomic_cmpxchg_acquire_relaxed(let t):
      return "atomic_cmpxchg_acquire_relaxed_\(t)"
    case .atomic_cmpxchg_acquire_acquire(let t):
      return "atomic_cmpxchg_acquire_acquire_\(t)"
    case .atomic_cmpxchg_acquire_seqcst(let t):
      return "atomic_cmpxchg_acquire_seqcst_\(t)"
    case .atomic_cmpxchg_release_relaxed(let t):
      return "atomic_cmpxchg_release_relaxed_\(t)"
    case .atomic_cmpxchg_release_acquire(let t):
      return "atomic_cmpxchg_release_acquire_\(t)"
    case .atomic_cmpxchg_release_seqcst(let t):
      return "atomic_cmpxchg_release_seqcst_\(t)"
    case .atomic_cmpxchg_acqrel_relaxed(let t):
      return "atomic_cmpxchg_acqrel_relaxed_\(t)"
    case .atomic_cmpxchg_acqrel_acquire(let t):
      return "atomic_cmpxchg_acqrel_acquire_\(t)"
    case .atomic_cmpxchg_acqrel_seqcst(let t):
      return "atomic_cmpxchg_acqrel_seqcst_\(t)"
    case .atomic_cmpxchg_seqcst_relaxed(let t):
      return "atomic_cmpxchg_seqcst_relaxed_\(t)"
    case .atomic_cmpxchg_seqcst_acquire(let t):
      return "atomic_cmpxchg_seqcst_acquire_\(t)"
    case .atomic_cmpxchg_seqcst_seqcst(let t):
      return "atomic_cmpxchg_seqcst_seqcst_\(t)"
    case .atomic_cmpxchgweak_relaxed_relaxed(let t):
      return "atomic_cmpxchgweak_relaxed_relaxed_\(t)"
    case .atomic_cmpxchgweak_relaxed_acquire(let t):
      return "atomic_cmpxchgweak_relaxed_acquire_\(t)"
    case .atomic_cmpxchgweak_relaxed_seqcst(let t):
      return "atomic_cmpxchgweak_relaxed_seqcst_\(t)"
    case .atomic_cmpxchgweak_acquire_relaxed(let t):
      return "atomic_cmpxchgweak_acquire_relaxed_\(t)"
    case .atomic_cmpxchgweak_acquire_acquire(let t):
      return "atomic_cmpxchgweak_acquire_acquire_\(t)"
    case .atomic_cmpxchgweak_acquire_seqcst(let t):
      return "atomic_cmpxchgweak_acquire_seqcst_\(t)"
    case .atomic_cmpxchgweak_release_relaxed(let t):
      return "atomic_cmpxchgweak_release_relaxed_\(t)"
    case .atomic_cmpxchgweak_release_acquire(let t):
      return "atomic_cmpxchgweak_release_acquire_\(t)"
    case .atomic_cmpxchgweak_release_seqcst(let t):
      return "atomic_cmpxchgweak_release_seqcst_\(t)"
    case .atomic_cmpxchgweak_acqrel_relaxed(let t):
      return "atomic_cmpxchgweak_acqrel_relaxed_\(t)"
    case .atomic_cmpxchgweak_acqrel_acquire(let t):
      return "atomic_cmpxchgweak_acqrel_acquire_\(t)"
    case .atomic_cmpxchgweak_acqrel_seqcst(let t):
      return "atomic_cmpxchgweak_acqrel_seqcst_\(t)"
    case .atomic_cmpxchgweak_seqcst_relaxed(let t):
      return "atomic_cmpxchgweak_seqcst_relaxed_\(t)"
    case .atomic_cmpxchgweak_seqcst_acquire(let t):
      return "atomic_cmpxchgweak_seqcst_acquire_\(t)"
    case .atomic_cmpxchgweak_seqcst_seqcst(let t):
      return "atomic_cmpxchgweak_seqcst_seqcst_\(t)"
    case .atomic_fence_acquire:
      return "atomic_fence_acquire"
    case .atomic_fence_release:
      return "atomic_fence_release"
    case .atomic_fence_acqrel:
      return "atomic_fence_acqrel"
    case .atomic_fence_seqcst:
      return "atomic_fence_seqcst"
    case .atomic_singlethreadfence_acquire:
      return "atomic_singlethreadfence_acquire"
    case .atomic_singlethreadfence_release:
      return "atomic_singlethreadfence_release"
    case .atomic_singlethreadfence_acqrel:
      return "atomic_singlethreadfence_acqrel"
    case .atomic_singlethreadfence_seqcst:
      return "atomic_singlethreadfence_seqcst"
    }
  }

}

extension BuiltinFunction.MathFlags: CustomStringConvertible {

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


// MARK: Parsing

extension BuiltinFunction {

  /// Creates a built-in function named `n` or returns `nil` if `n` isn't a valid name.
  public init?(_ n: String) {
    var tokens = n.split(separator: "_")[...]

    // The first token is the LLVM instruction name.
    guard let head = tokens.popFirst() else { return nil }
    switch head {
    case "address":
      if !tokens.isEmpty { return nil }
      self = .addressOf

    case "mark":
      if tokens != ["uninitialized"] { return nil }
      self = .markUninitialized

    case "advanced":
      guard let ((_, _), t) = (exactly("by") ++ exactly("bytes") ++ builtinType)(&tokens)
      else { return nil }
      self = .advancedByBytes(byteOffset: t)

    case "add":
      guard let (p, t) = integerArithmeticTail(&tokens) else { return nil }
      self = .add(p, t)

    case "sub":
      guard let (p, t) = integerArithmeticTail(&tokens) else { return nil }
      self = .sub(p, t)

    case "mul":
      guard let (p, t) = integerArithmeticTail(&tokens) else { return nil }
      self = .mul(p, t)

    case "shl":
      guard let (p, t) = integerArithmeticTail(&tokens) else { return nil }
      self = .shl(p, t)

    case "udiv":
      guard let (p, t) = (maybe("exact") ++ builtinType)(&tokens) else { return nil }
      self = .udiv(exact: p != nil, t)

    case "sdiv":
      guard let (p, t) = (maybe("exact") ++ builtinType)(&tokens) else { return nil }
      self = .sdiv(exact: p != nil, t)

    case "lshr":
      guard let (p, t) = (maybe("exact") ++ builtinType)(&tokens) else { return nil }
      self = .lshr(exact: p != nil, t)

    case "ashr":
      guard let (p, t) = (maybe("exact") ++ builtinType)(&tokens) else { return nil }
      self = .ashr(exact: p != nil, t)

    case "urem":
      guard let t = builtinType(&tokens) else { return nil }
      self = .urem(t)

    case "srem":
      guard let t = builtinType(&tokens) else { return nil }
      self = .srem(t)

    case "and":
      guard let t = builtinType(&tokens) else { return nil }
      self = .and(t)

    case "or":
      guard let t = builtinType(&tokens) else { return nil }
      self = .or(t)

    case "xor":
      guard let t = builtinType(&tokens) else { return nil }
      self = .xor(t)

    case "sadd":
      guard let t = integerArithmeticWithOverflowTail(&tokens) else { return nil }
      self = .signedAdditionWithOverflow(t)

    case "uadd":
      guard let t = integerArithmeticWithOverflowTail(&tokens) else { return nil }
      self = .unsignedAdditionWithOverflow(t)

    case "ssub":
      guard let t = integerArithmeticWithOverflowTail(&tokens) else { return nil }
      self = .signedSubtractionWithOverflow(t)

    case "usub":
      guard let t = integerArithmeticWithOverflowTail(&tokens) else { return nil }
      self = .unsignedSubtractionWithOverflow(t)

    case "smul":
      guard let t = integerArithmeticWithOverflowTail(&tokens) else { return nil }
      self = .signedMultiplicationWithOverflow(t)

    case "umul":
      guard let t = integerArithmeticWithOverflowTail(&tokens) else { return nil }
      self = .unsignedMultiplicationWithOverflow(t)

    case "icmp":
      guard let (p, t) = integerComparisonTail(&tokens) else { return nil }
      self = .icmp(p, t)

    case "trunc":
      guard let (s, d) = (builtinType ++ builtinType)(&tokens) else { return nil }
      self = .trunc(s, d)

    case "zext":
      guard let (s, d) = (builtinType ++ builtinType)(&tokens) else { return nil }
      self = .zext(s, d)

    case "sext":
      guard let (s, d) = (builtinType ++ builtinType)(&tokens) else { return nil }
      self = .sext(s, d)

    case "uitofp":
      guard let (s, d) = (builtinType ++ builtinType)(&tokens) else { return nil }
      self = .uitofp(s, d)

    case "sitofp":
      guard let (s, d) = (builtinType ++ builtinType)(&tokens) else { return nil }
      self = .sitofp(s, d)

    case "inttoptr":
      guard let t = builtinType(&tokens) else { return nil }
      self = .inttoptr(t)

    case "ptrtoint":
      guard let t = builtinType(&tokens) else { return nil }
      self = .ptrtoint(t)

    case "fadd":
      guard let (p, t) = floatingPointArithmeticTail(&tokens) else { return nil }
      self = .fadd(p, t)

    case "fsub":
      guard let (p, t) = floatingPointArithmeticTail(&tokens) else { return nil }
      self = .fsub(p, t)

    case "fmul":
      guard let (p, t) = floatingPointArithmeticTail(&tokens) else { return nil }
      self = .fmul(p, t)

    case "fdiv":
      guard let (p, t) = floatingPointArithmeticTail(&tokens) else { return nil }
      self = .fdiv(p, t)

    case "frem":
      guard let (p, t) = floatingPointArithmeticTail(&tokens) else { return nil }
      self = .frem(p, t)

    case "fcmp":
      guard let (p, t) = floatingPointComparisonTail(&tokens) else { return nil }
      self = .fcmp(p.0, p.1, t)

    case "fptrunc":
      guard let (s, d) = (builtinType ++ builtinType)(&tokens) else { return nil }
      self = .fptrunc(s, d)

    case "fpext":
      guard let (s, d) = (builtinType ++ builtinType)(&tokens) else { return nil }
      self = .fpext(s, d)

    case "fptoui":
      guard let (s, d) = (builtinType ++ builtinType)(&tokens) else { return nil }
      self = .fptoui(s, d)

    case "fptosi":
      guard let (s, d) = (builtinType ++ builtinType)(&tokens) else { return nil }
      self = .fptosi(s, d)

    case "ctpop":
      guard let t = builtinType(&tokens) else { return nil }
      self = .ctpop(t)

    case "ctlz":
      guard let t = builtinType(&tokens) else { return nil }
      self = .ctlz(t)

    case "cttz":
      guard let t = builtinType(&tokens) else { return nil }
      self = .cttz(t)

    case "zeroinitializer":
      guard let t = builtinType(&tokens) else { return nil }
      self = .zeroinitializer(t)

    case "atomic":
      if let t = tokens.first, t.contains("fence") {
        self.init(fence: n)
      } else {
        self.init(atomic: n)
      }

    default:
      return nil
    }
  }

  /// Creates a built-in function representing the native fence instruction named `n` or returns
  /// `nil` if `n` isn't a valid native fence instruction name.
  private init?(fence n: String) {
    switch n {
    case "atomic_fence_acquire":
      self = .atomic_fence_acquire
    case "atomic_fence_release":
      self = .atomic_fence_release
    case "atomic_fence_acqrel":
      self = .atomic_fence_acqrel
    case "atomic_fence_seqcst":
      self = .atomic_fence_seqcst
    case "atomic_singlethreadfence_acquire":
      self = .atomic_singlethreadfence_acquire
    case "atomic_singlethreadfence_release":
      self = .atomic_singlethreadfence_release
    case "atomic_singlethreadfence_acqrel":
      self = .atomic_singlethreadfence_acqrel
    case "atomic_singlethreadfence_seqcst":
      self = .atomic_singlethreadfence_seqcst
    default:
      return nil
    }
  }

  /// Creates a built-in function representing the native atomic instruction named `n` or returns
  /// `nil` if `n` isn't a valid native atomic instruction name.
  private init?(atomic n: String) {
    // The type of all atomics (except fences) is mentioned at the end.
    let m = n.split(atLastIndexOf: "_")
    guard let t = BuiltinType(m.tail.dropFirst()) else { return nil }

    switch m.head {
    case "atomic_store_relaxed":
      self = .atomic_store_relaxed(t)
    case "atomic_store_release":
      self = .atomic_store_release(t)
    case "atomic_store_seqcst":
      self = .atomic_store_seqcst(t)
    case "atomic_load_relaxed":
      self = .atomic_load_relaxed(t)
    case "atomic_load_acquire":
      self = .atomic_load_acquire(t)
    case "atomic_load_seqcst":
      self = .atomic_load_seqcst(t)
    case "atomic_swap_relaxed":
      self = .atomic_swap_relaxed(t)
    case "atomic_swap_acquire":
      self = .atomic_swap_acquire(t)
    case "atomic_swap_release":
      self = .atomic_swap_release(t)
    case "atomic_swap_acqrel":
      self = .atomic_swap_acqrel(t)
    case "atomic_swap_seqcst":
      self = .atomic_swap_seqcst(t)
    case "atomic_add_relaxed":
      self = .atomic_add_relaxed(t)
    case "atomic_add_acquire":
      self = .atomic_add_acquire(t)
    case "atomic_add_release":
      self = .atomic_add_release(t)
    case "atomic_add_acqrel":
      self = .atomic_add_acqrel(t)
    case "atomic_add_seqcst":
      self = .atomic_add_seqcst(t)
    case "atomic_fadd_relaxed":
      self = .atomic_fadd_relaxed(t)
    case "atomic_fadd_acquire":
      self = .atomic_fadd_acquire(t)
    case "atomic_fadd_release":
      self = .atomic_fadd_release(t)
    case "atomic_fadd_acqrel":
      self = .atomic_fadd_acqrel(t)
    case "atomic_fadd_seqcst":
      self = .atomic_fadd_seqcst(t)
    case "atomic_sub_relaxed":
      self = .atomic_sub_relaxed(t)
    case "atomic_sub_acquire":
      self = .atomic_sub_acquire(t)
    case "atomic_sub_release":
      self = .atomic_sub_release(t)
    case "atomic_sub_acqrel":
      self = .atomic_sub_acqrel(t)
    case "atomic_sub_seqcst":
      self = .atomic_sub_seqcst(t)
    case "atomic_fsub_relaxed":
      self = .atomic_fsub_relaxed(t)
    case "atomic_fsub_acquire":
      self = .atomic_fsub_acquire(t)
    case "atomic_fsub_release":
      self = .atomic_fsub_release(t)
    case "atomic_fsub_acqrel":
      self = .atomic_fsub_acqrel(t)
    case "atomic_fsub_seqcst":
      self = .atomic_fsub_seqcst(t)
    case "atomic_max_relaxed":
      self = .atomic_max_relaxed(t)
    case "atomic_max_acquire":
      self = .atomic_max_acquire(t)
    case "atomic_max_release":
      self = .atomic_max_release(t)
    case "atomic_max_acqrel":
      self = .atomic_max_acqrel(t)
    case "atomic_max_seqcst":
      self = .atomic_max_seqcst(t)
    case "atomic_umax_relaxed":
      self = .atomic_umax_relaxed(t)
    case "atomic_umax_acquire":
      self = .atomic_umax_acquire(t)
    case "atomic_umax_release":
      self = .atomic_umax_release(t)
    case "atomic_umax_acqrel":
      self = .atomic_umax_acqrel(t)
    case "atomic_umax_seqcst":
      self = .atomic_umax_seqcst(t)
    case "atomic_fmax_relaxed":
      self = .atomic_fmax_relaxed(t)
    case "atomic_fmax_acquire":
      self = .atomic_fmax_acquire(t)
    case "atomic_fmax_release":
      self = .atomic_fmax_release(t)
    case "atomic_fmax_acqrel":
      self = .atomic_fmax_acqrel(t)
    case "atomic_fmax_seqcst":
      self = .atomic_fmax_seqcst(t)
    case "atomic_min_relaxed":
      self = .atomic_min_relaxed(t)
    case "atomic_min_acquire":
      self = .atomic_min_acquire(t)
    case "atomic_min_release":
      self = .atomic_min_release(t)
    case "atomic_min_acqrel":
      self = .atomic_min_acqrel(t)
    case "atomic_min_seqcst":
      self = .atomic_min_seqcst(t)
    case "atomic_umin_relaxed":
      self = .atomic_umin_relaxed(t)
    case "atomic_umin_acquire":
      self = .atomic_umin_acquire(t)
    case "atomic_umin_release":
      self = .atomic_umin_release(t)
    case "atomic_umin_acqrel":
      self = .atomic_umin_acqrel(t)
    case "atomic_umin_seqcst":
      self = .atomic_umin_seqcst(t)
    case "atomic_fmin_relaxed":
      self = .atomic_fmin_relaxed(t)
    case "atomic_fmin_acquire":
      self = .atomic_fmin_acquire(t)
    case "atomic_fmin_release":
      self = .atomic_fmin_release(t)
    case "atomic_fmin_acqrel":
      self = .atomic_fmin_acqrel(t)
    case "atomic_fmin_seqcst":
      self = .atomic_fmin_seqcst(t)
    case "atomic_and_relaxed":
      self = .atomic_and_relaxed(t)
    case "atomic_and_acquire":
      self = .atomic_and_acquire(t)
    case "atomic_and_release":
      self = .atomic_and_release(t)
    case "atomic_and_acqrel":
      self = .atomic_and_acqrel(t)
    case "atomic_and_seqcst":
      self = .atomic_and_seqcst(t)
    case "atomic_nand_relaxed":
      self = .atomic_nand_relaxed(t)
    case "atomic_nand_acquire":
      self = .atomic_nand_acquire(t)
    case "atomic_nand_release":
      self = .atomic_nand_release(t)
    case "atomic_nand_acqrel":
      self = .atomic_nand_acqrel(t)
    case "atomic_nand_seqcst":
      self = .atomic_nand_seqcst(t)
    case "atomic_or_relaxed":
      self = .atomic_or_relaxed(t)
    case "atomic_or_acquire":
      self = .atomic_or_acquire(t)
    case "atomic_or_release":
      self = .atomic_or_release(t)
    case "atomic_or_acqrel":
      self = .atomic_or_acqrel(t)
    case "atomic_or_seqcst":
      self = .atomic_or_seqcst(t)
    case "atomic_xor_relaxed":
      self = .atomic_xor_relaxed(t)
    case "atomic_xor_acquire":
      self = .atomic_xor_acquire(t)
    case "atomic_xor_release":
      self = .atomic_xor_release(t)
    case "atomic_xor_acqrel":
      self = .atomic_xor_acqrel(t)
    case "atomic_xor_seqcst":
      self = .atomic_xor_seqcst(t)
    case "atomic_cmpxchg_relaxed_relaxed":
      self = .atomic_cmpxchg_relaxed_relaxed(t)
    case "atomic_cmpxchg_relaxed_acquire":
      self = .atomic_cmpxchg_relaxed_acquire(t)
    case "atomic_cmpxchg_relaxed_seqcst":
      self = .atomic_cmpxchg_relaxed_seqcst(t)
    case "atomic_cmpxchg_acquire_relaxed":
      self = .atomic_cmpxchg_acquire_relaxed(t)
    case "atomic_cmpxchg_acquire_acquire":
      self = .atomic_cmpxchg_acquire_acquire(t)
    case "atomic_cmpxchg_acquire_seqcst":
      self = .atomic_cmpxchg_acquire_seqcst(t)
    case "atomic_cmpxchg_release_relaxed":
      self = .atomic_cmpxchg_release_relaxed(t)
    case "atomic_cmpxchg_release_acquire":
      self = .atomic_cmpxchg_release_acquire(t)
    case "atomic_cmpxchg_release_seqcst":
      self = .atomic_cmpxchg_release_seqcst(t)
    case "atomic_cmpxchg_acqrel_relaxed":
      self = .atomic_cmpxchg_acqrel_relaxed(t)
    case "atomic_cmpxchg_acqrel_acquire":
      self = .atomic_cmpxchg_acqrel_acquire(t)
    case "atomic_cmpxchg_acqrel_seqcst":
      self = .atomic_cmpxchg_acqrel_seqcst(t)
    case "atomic_cmpxchg_seqcst_relaxed":
      self = .atomic_cmpxchg_seqcst_relaxed(t)
    case "atomic_cmpxchg_seqcst_acquire":
      self = .atomic_cmpxchg_seqcst_acquire(t)
    case "atomic_cmpxchg_seqcst_seqcst":
      self = .atomic_cmpxchg_seqcst_seqcst(t)
    case "atomic_cmpxchgweak_relaxed_relaxed":
      self = .atomic_cmpxchgweak_relaxed_relaxed(t)
    case "atomic_cmpxchgweak_relaxed_acquire":
      self = .atomic_cmpxchgweak_relaxed_acquire(t)
    case "atomic_cmpxchgweak_relaxed_seqcst":
      self = .atomic_cmpxchgweak_relaxed_seqcst(t)
    case "atomic_cmpxchgweak_acquire_relaxed":
      self = .atomic_cmpxchgweak_acquire_relaxed(t)
    case "atomic_cmpxchgweak_acquire_acquire":
      self = .atomic_cmpxchgweak_acquire_acquire(t)
    case "atomic_cmpxchgweak_acquire_seqcst":
      self = .atomic_cmpxchgweak_acquire_seqcst(t)
    case "atomic_cmpxchgweak_release_relaxed":
      self = .atomic_cmpxchgweak_release_relaxed(t)
    case "atomic_cmpxchgweak_release_acquire":
      self = .atomic_cmpxchgweak_release_acquire(t)
    case "atomic_cmpxchgweak_release_seqcst":
      self = .atomic_cmpxchgweak_release_seqcst(t)
    case "atomic_cmpxchgweak_acqrel_relaxed":
      self = .atomic_cmpxchgweak_acqrel_relaxed(t)
    case "atomic_cmpxchgweak_acqrel_acquire":
      self = .atomic_cmpxchgweak_acqrel_acquire(t)
    case "atomic_cmpxchgweak_acqrel_seqcst":
      self = .atomic_cmpxchgweak_acqrel_seqcst(t)
    case "atomic_cmpxchgweak_seqcst_relaxed":
      self = .atomic_cmpxchgweak_seqcst_relaxed(t)
    case "atomic_cmpxchgweak_seqcst_acquire":
      self = .atomic_cmpxchgweak_seqcst_acquire(t)
    case "atomic_cmpxchgweak_seqcst_seqcst":
      self = .atomic_cmpxchgweak_seqcst_seqcst(t)
    default:
      return nil
    }
  }

}

extension BuiltinFunction.MathFlags {

  /// Creates the math flag described by `description`.
  fileprivate init?(description: Substring) {
    switch description {
    case "afn":
      self = .afn
    case "arcp":
      self = .arcp
    case "contract":
      self = .contract
    case "fast":
      self = .fast
    case "ninf":
      self = .ninf
    case "nnan":
      self = .nnan
    case "nsz":
      self = .nsz
    case "reassoc":
      self = .reassoc
    default:
      return nil
    }
  }

}

/// A function that parses an instance of `T` by consuming a prefix of `tokens`, or returns `nil`
/// if such instance cannot be parsed.
///
/// - Note: a prefix of `tokens` may have been consumed even if the function returns `nil`.
private typealias BuiltinFunctionParser<T> = (_ tokens: inout ArraySlice<Substring>) -> T?

/// Returns a parser that consumes an element equal to `s` and returns `.some(s)`, or returns
/// `.some(nil)` if such an element can't be consumed.
private func maybe(_ s: String) -> BuiltinFunctionParser<String?> {
  { (stream: inout ArraySlice<Substring>) -> String?? in
    if let r = exactly(s)(&stream) { return .some(r) }
    return .some(nil)
  }
}

/// Returns a parser that consumes and returns an element equal to `s`.
private func exactly(_ s: String) -> BuiltinFunctionParser<String> {
  { (stream: inout ArraySlice<Substring>) -> String? in
    stream.first == s[...] ? (stream.popFirst(), .some(s)).1 : nil
  }
}

/// Returns a parser that returns the result of applying `a` and then `b` or `nil` if either `a`
/// or `b` returns `nil`.
private func ++ <A, B>(
  _ a: @escaping BuiltinFunctionParser<A>, _ b: @escaping BuiltinFunctionParser<B>
) -> BuiltinFunctionParser<(A, B)> {
  { (stream: inout ArraySlice<Substring>) -> (A, B)? in
    a(&stream).flatMap({ (x) in b(&stream).map({ (x, $0) }) })
  }
}

/// Returns a parser that returns an instance of `T` if it can be built by consuming the next
/// element in the stream.
private func take<T: RawRepresentable>(
  _: T.Type
) -> BuiltinFunctionParser<T> where T.RawValue == String {
  { (stream: inout ArraySlice<Substring>) -> T? in
    stream.popFirst().flatMap({ T(rawValue: .init($0)) })
  }
}

/// Returns a built-in type parsed from `stream`.
private func builtinType(_ stream: inout ArraySlice<Substring>) -> BuiltinType? {
  stream.popFirst().flatMap(BuiltinType.init(_:))
}

/// Returns the longest sequence of floating-point math flags that can be parsed from `stream`.
private func mathFlags(_ stream: inout ArraySlice<Substring>) -> BuiltinFunction.MathFlags {
  var result: BuiltinFunction.MathFlags = []
  while let x = stream.first {
    guard let y = BuiltinFunction.MathFlags(description: x) else { break }
    stream.removeFirst()
    result.insert(y)
  }
  return result
}

/// Returns an overflow behavior parsed from `stream` or `.ignore` if none can be parsed.
private func overflowBehavior(
  _ stream: inout ArraySlice<Substring>
) -> OverflowBehavior {
  switch stream.first {
  case "nuw":
    stream.removeFirst()
    return .nuw
  case "nsw":
    stream.removeFirst()
    return .nsw
  default:
    return .ignore
  }
}

/// Parses the parameters and type of an integer arithmetic instruction with overflow reporting.
private func integerArithmeticWithOverflowTail(
  _ stream: inout ArraySlice<Substring>
) -> BuiltinType? {
  let p = exactly("with") ++ exactly("overflow") ++ builtinType
  return p(&stream).map(\.1)
}

/// Parses the parameters and type of an integer arithmetic instruction.
private let integerArithmeticTail =
  overflowBehavior ++ builtinType

/// Parses the parameters and type of `icmp`.
private let integerComparisonTail =
  take(IntegerPredicate.self) ++ builtinType

/// Parses the parameters and type of a floating-point arithmetic instruction.
private let floatingPointArithmeticTail =
  mathFlags ++ builtinType

/// Parses the parameters and type of `fcmp`.
private let floatingPointComparisonTail =
  mathFlags ++ take(FloatingPointPredicate.self) ++ builtinType
