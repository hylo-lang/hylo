import Utils

/// A function representing an IR instruction in Hylo source code.
public struct BuiltinFunction: Hashable {

  /// The name of the function.
  public let name: Name

  /// Returns the type of the function, calling `freshVariable` to create fresh type variables.
  public func type(makingFreshVariableWith freshVariable: () -> TypeVariable) -> ArrowType {
    switch self.name {
    case .addressOf:
      let p = ParameterType(.let, ^freshVariable())
      return .init(inputs: [.init(label: "of", type: ^p)], output: .builtin(.ptr))

    case .markUninitialized:
      let p = ParameterType(.let, ^freshVariable())
      return .init(inputs: [.init(label: nil, type: ^p)], output: .void)

    case .llvm(let s):
      return s.type
    }
  }

}

extension BuiltinFunction {

  /// The name of a built-in function.
  public enum Name: Hashable {

    /// An LLVM instruction.
    case llvm(NativeInstruction)

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

  }

}

extension BuiltinFunction.Name: CustomStringConvertible {

  public var description: String {
    switch self {
    case .llvm(let n):
      return n.description
    case .addressOf:
      return "address(of:)"
    case .markUninitialized:
      return "mark_uninitialized(_:)"
    }
  }

}

// MARK: Parsing

extension BuiltinFunction {

  /// Creates a built-in function named `n` or returns `nil` if `n` isn't a valid name.
  public init?(_ n: String) {
    switch n {
    case "address":
      self.init(name: .addressOf)
    case "mark_uninitialized":
      self.init(name: .markUninitialized)
    default:
      self.init(native: n)
    }
  }

  /// Creates a built-in function representing the native instruction named `n` or returns `nil`
  /// if `n` isn't a valid native instruction name.
  private init?(native n: String) {
    var tokens = n.split(separator: "_")[...]

    // The first token is the LLVM instruction name.
    guard let head = tokens.popFirst() else { return nil }
    switch head {
    case "advanced":
      guard let ((_, _), t) = (exactly("by") ++ exactly("bytes") ++ builtinType)(&tokens)
      else { return nil }
      self = .init(name: .llvm(.advancedByBytes(byteOffset: t)))

    case "add":
      guard let (p, t) = integerArithmeticTail(&tokens) else { return nil }
      self = .init(name: .llvm(.add(p, t)))

    case "sub":
      guard let (p, t) = integerArithmeticTail(&tokens) else { return nil }
      self = .init(name: .llvm(.sub(p, t)))

    case "mul":
      guard let (p, t) = integerArithmeticTail(&tokens) else { return nil }
      self = .init(name: .llvm(.mul(p, t)))

    case "shl":
      guard let (p, t) = integerArithmeticTail(&tokens) else { return nil }
      self = .init(name: .llvm(.shl(p, t)))

    case "udiv":
      guard let (p, t) = (maybe("exact") ++ builtinType)(&tokens) else { return nil }
      self = .init(name: .llvm(.udiv(exact: p != nil, t)))

    case "sdiv":
      guard let (p, t) = (maybe("exact") ++ builtinType)(&tokens) else { return nil }
      self = .init(name: .llvm(.sdiv(exact: p != nil, t)))

    case "lshr":
      guard let (p, t) = (maybe("exact") ++ builtinType)(&tokens) else { return nil }
      self = .init(name: .llvm(.lshr(exact: p != nil, t)))

    case "ashr":
      guard let (p, t) = (maybe("exact") ++ builtinType)(&tokens) else { return nil }
      self = .init(name: .llvm(.ashr(exact: p != nil, t)))

    case "urem":
      guard let t = builtinType(&tokens) else { return nil }
      self = .init(name: .llvm(.urem(t)))

    case "srem":
      guard let t = builtinType(&tokens) else { return nil }
      self = .init(name: .llvm(.srem(t)))

    case "and":
      guard let t = builtinType(&tokens) else { return nil }
      self = .init(name: .llvm(.and(t)))

    case "or":
      guard let t = builtinType(&tokens) else { return nil }
      self = .init(name: .llvm(.or(t)))

    case "xor":
      guard let t = builtinType(&tokens) else { return nil }
      self = .init(name: .llvm(.xor(t)))

    case "sadd":
      guard let t = integerArithmeticWithOverflowTail(&tokens) else { return nil }
      self = .init(name: .llvm(.signedAdditionWithOverflow(t)))

    case "uadd":
      guard let t = integerArithmeticWithOverflowTail(&tokens) else { return nil }
      self = .init(name: .llvm(.unsignedAdditionWithOverflow(t)))

    case "ssub":
      guard let t = integerArithmeticWithOverflowTail(&tokens) else { return nil }
      self = .init(name: .llvm(.signedSubtractionWithOverflow(t)))

    case "usub":
      guard let t = integerArithmeticWithOverflowTail(&tokens) else { return nil }
      self = .init(name: .llvm(.unsignedSubtractionWithOverflow(t)))

    case "smul":
      guard let t = integerArithmeticWithOverflowTail(&tokens) else { return nil }
      self = .init(name: .llvm(.signedMultiplicationWithOverflow(t)))

    case "umul":
      guard let t = integerArithmeticWithOverflowTail(&tokens) else { return nil }
      self = .init(name: .llvm(.unsignedMultiplicationWithOverflow(t)))

    case "icmp":
      guard let (p, t) = integerComparisonTail(&tokens) else { return nil }
      self = .init(name: .llvm(.icmp(p, t)))

    case "trunc":
      guard let (s, d) = (builtinType ++ builtinType)(&tokens) else { return nil }
      self = .init(name: .llvm(.trunc(s, d)))

    case "zext":
      guard let (s, d) = (builtinType ++ builtinType)(&tokens) else { return nil }
      self = .init(name: .llvm(.zext(s, d)))

    case "sext":
      guard let (s, d) = (builtinType ++ builtinType)(&tokens) else { return nil }
      self = .init(name: .llvm(.sext(s, d)))

    case "uitofp":
      guard let (s, d) = (builtinType ++ builtinType)(&tokens) else { return nil }
      self = .init(name: .llvm(.uitofp(s, d)))

    case "sitofp":
      guard let (s, d) = (builtinType ++ builtinType)(&tokens) else { return nil }
      self = .init(name: .llvm(.sitofp(s, d)))

    case "inttoptr":
      guard let t = builtinType(&tokens) else { return nil }
      self = .init(name: .llvm(.inttoptr(t)))

    case "ptrtoint":
      guard let t = builtinType(&tokens) else { return nil }
      self = .init(name: .llvm(.ptrtoint(t)))

    case "fadd":
      guard let (p, t) = floatingPointArithmeticTail(&tokens) else { return nil }
      self = .init(name: .llvm(.fadd(p, t)))

    case "fsub":
      guard let (p, t) = floatingPointArithmeticTail(&tokens) else { return nil }
      self = .init(name: .llvm(.fsub(p, t)))

    case "fmul":
      guard let (p, t) = floatingPointArithmeticTail(&tokens) else { return nil }
      self = .init(name: .llvm(.fmul(p, t)))

    case "fdiv":
      guard let (p, t) = floatingPointArithmeticTail(&tokens) else { return nil }
      self = .init(name: .llvm(.fdiv(p, t)))

    case "frem":
      guard let (p, t) = floatingPointArithmeticTail(&tokens) else { return nil }
      self = .init(name: .llvm(.frem(p, t)))

    case "fcmp":
      guard let (p, t) = floatingPointComparisonTail(&tokens) else { return nil }
      self = .init(name: .llvm(.fcmp(p.0, p.1, t)))

    case "fptrunc":
      guard let (s, d) = (builtinType ++ builtinType)(&tokens) else { return nil }
      self = .init(name: .llvm(.fptrunc(s, d)))

    case "fpext":
      guard let (s, d) = (builtinType ++ builtinType)(&tokens) else { return nil }
      self = .init(name: .llvm(.fpext(s, d)))

    case "fptoui":
      guard let (s, d) = (builtinType ++ builtinType)(&tokens) else { return nil }
      self = .init(name: .llvm(.fptoui(s, d)))

    case "fptosi":
      guard let (s, d) = (builtinType ++ builtinType)(&tokens) else { return nil }
      self = .init(name: .llvm(.fptosi(s, d)))

    case "ctpop":
      guard let t = builtinType(&tokens) else { return nil }
      self = .init(name: .llvm(.ctpop(t)))

    case "ctlz":
      guard let t = builtinType(&tokens) else { return nil }
      self = .init(name: .llvm(.ctlz(t)))

    case "cttz":
      guard let t = builtinType(&tokens) else { return nil }
      self = .init(name: .llvm(.cttz(t)))

    case "zeroinitializer":
      guard let t = builtinType(&tokens) else { return nil }
      self = .init(name: .llvm(.zeroinitializer(t)))

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
      self = .init(name: .llvm(.atomic_fence_acquire))
    case "atomic_fence_release":
      self = .init(name: .llvm(.atomic_fence_release))
    case "atomic_fence_acqrel":
      self = .init(name: .llvm(.atomic_fence_acqrel))
    case "atomic_fence_seqcst":
      self = .init(name: .llvm(.atomic_fence_seqcst))
    case "atomic_singlethreadfence_acquire":
      self = .init(name: .llvm(.atomic_singlethreadfence_acquire))
    case "atomic_singlethreadfence_release":
      self = .init(name: .llvm(.atomic_singlethreadfence_release))
    case "atomic_singlethreadfence_acqrel":
      self = .init(name: .llvm(.atomic_singlethreadfence_acqrel))
    case "atomic_singlethreadfence_seqcst":
      self = .init(name: .llvm(.atomic_singlethreadfence_seqcst))
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
      self = .init(name: .llvm(.atomic_store_relaxed(t)))
    case "atomic_store_release":
      self = .init(name: .llvm(.atomic_store_release(t)))
    case "atomic_store_seqcst":
      self = .init(name: .llvm(.atomic_store_seqcst(t)))
    case "atomic_load_relaxed":
      self = .init(name: .llvm(.atomic_load_relaxed(t)))
    case "atomic_load_acquire":
      self = .init(name: .llvm(.atomic_load_acquire(t)))
    case "atomic_load_seqcst":
      self = .init(name: .llvm(.atomic_load_seqcst(t)))
    case "atomic_swap_relaxed":
      self = .init(name: .llvm(.atomic_swap_relaxed(t)))
    case "atomic_swap_acquire":
      self = .init(name: .llvm(.atomic_swap_acquire(t)))
    case "atomic_swap_release":
      self = .init(name: .llvm(.atomic_swap_release(t)))
    case "atomic_swap_acqrel":
      self = .init(name: .llvm(.atomic_swap_acqrel(t)))
    case "atomic_swap_seqcst":
      self = .init(name: .llvm(.atomic_swap_seqcst(t)))
    case "atomic_add_relaxed":
      self = .init(name: .llvm(.atomic_add_relaxed(t)))
    case "atomic_add_acquire":
      self = .init(name: .llvm(.atomic_add_acquire(t)))
    case "atomic_add_release":
      self = .init(name: .llvm(.atomic_add_release(t)))
    case "atomic_add_acqrel":
      self = .init(name: .llvm(.atomic_add_acqrel(t)))
    case "atomic_add_seqcst":
      self = .init(name: .llvm(.atomic_add_seqcst(t)))
    case "atomic_fadd_relaxed":
      self = .init(name: .llvm(.atomic_fadd_relaxed(t)))
    case "atomic_fadd_acquire":
      self = .init(name: .llvm(.atomic_fadd_acquire(t)))
    case "atomic_fadd_release":
      self = .init(name: .llvm(.atomic_fadd_release(t)))
    case "atomic_fadd_acqrel":
      self = .init(name: .llvm(.atomic_fadd_acqrel(t)))
    case "atomic_fadd_seqcst":
      self = .init(name: .llvm(.atomic_fadd_seqcst(t)))
    case "atomic_sub_relaxed":
      self = .init(name: .llvm(.atomic_sub_relaxed(t)))
    case "atomic_sub_acquire":
      self = .init(name: .llvm(.atomic_sub_acquire(t)))
    case "atomic_sub_release":
      self = .init(name: .llvm(.atomic_sub_release(t)))
    case "atomic_sub_acqrel":
      self = .init(name: .llvm(.atomic_sub_acqrel(t)))
    case "atomic_sub_seqcst":
      self = .init(name: .llvm(.atomic_sub_seqcst(t)))
    case "atomic_fsub_relaxed":
      self = .init(name: .llvm(.atomic_fsub_relaxed(t)))
    case "atomic_fsub_acquire":
      self = .init(name: .llvm(.atomic_fsub_acquire(t)))
    case "atomic_fsub_release":
      self = .init(name: .llvm(.atomic_fsub_release(t)))
    case "atomic_fsub_acqrel":
      self = .init(name: .llvm(.atomic_fsub_acqrel(t)))
    case "atomic_fsub_seqcst":
      self = .init(name: .llvm(.atomic_fsub_seqcst(t)))
    case "atomic_max_relaxed":
      self = .init(name: .llvm(.atomic_max_relaxed(t)))
    case "atomic_max_acquire":
      self = .init(name: .llvm(.atomic_max_acquire(t)))
    case "atomic_max_release":
      self = .init(name: .llvm(.atomic_max_release(t)))
    case "atomic_max_acqrel":
      self = .init(name: .llvm(.atomic_max_acqrel(t)))
    case "atomic_max_seqcst":
      self = .init(name: .llvm(.atomic_max_seqcst(t)))
    case "atomic_umax_relaxed":
      self = .init(name: .llvm(.atomic_umax_relaxed(t)))
    case "atomic_umax_acquire":
      self = .init(name: .llvm(.atomic_umax_acquire(t)))
    case "atomic_umax_release":
      self = .init(name: .llvm(.atomic_umax_release(t)))
    case "atomic_umax_acqrel":
      self = .init(name: .llvm(.atomic_umax_acqrel(t)))
    case "atomic_umax_seqcst":
      self = .init(name: .llvm(.atomic_umax_seqcst(t)))
    case "atomic_fmax_relaxed":
      self = .init(name: .llvm(.atomic_fmax_relaxed(t)))
    case "atomic_fmax_acquire":
      self = .init(name: .llvm(.atomic_fmax_acquire(t)))
    case "atomic_fmax_release":
      self = .init(name: .llvm(.atomic_fmax_release(t)))
    case "atomic_fmax_acqrel":
      self = .init(name: .llvm(.atomic_fmax_acqrel(t)))
    case "atomic_fmax_seqcst":
      self = .init(name: .llvm(.atomic_fmax_seqcst(t)))
    case "atomic_min_relaxed":
      self = .init(name: .llvm(.atomic_min_relaxed(t)))
    case "atomic_min_acquire":
      self = .init(name: .llvm(.atomic_min_acquire(t)))
    case "atomic_min_release":
      self = .init(name: .llvm(.atomic_min_release(t)))
    case "atomic_min_acqrel":
      self = .init(name: .llvm(.atomic_min_acqrel(t)))
    case "atomic_min_seqcst":
      self = .init(name: .llvm(.atomic_min_seqcst(t)))
    case "atomic_umin_relaxed":
      self = .init(name: .llvm(.atomic_umin_relaxed(t)))
    case "atomic_umin_acquire":
      self = .init(name: .llvm(.atomic_umin_acquire(t)))
    case "atomic_umin_release":
      self = .init(name: .llvm(.atomic_umin_release(t)))
    case "atomic_umin_acqrel":
      self = .init(name: .llvm(.atomic_umin_acqrel(t)))
    case "atomic_umin_seqcst":
      self = .init(name: .llvm(.atomic_umin_seqcst(t)))
    case "atomic_fmin_relaxed":
      self = .init(name: .llvm(.atomic_fmin_relaxed(t)))
    case "atomic_fmin_acquire":
      self = .init(name: .llvm(.atomic_fmin_acquire(t)))
    case "atomic_fmin_release":
      self = .init(name: .llvm(.atomic_fmin_release(t)))
    case "atomic_fmin_acqrel":
      self = .init(name: .llvm(.atomic_fmin_acqrel(t)))
    case "atomic_fmin_seqcst":
      self = .init(name: .llvm(.atomic_fmin_seqcst(t)))
    case "atomic_and_relaxed":
      self = .init(name: .llvm(.atomic_and_relaxed(t)))
    case "atomic_and_acquire":
      self = .init(name: .llvm(.atomic_and_acquire(t)))
    case "atomic_and_release":
      self = .init(name: .llvm(.atomic_and_release(t)))
    case "atomic_and_acqrel":
      self = .init(name: .llvm(.atomic_and_acqrel(t)))
    case "atomic_and_seqcst":
      self = .init(name: .llvm(.atomic_and_seqcst(t)))
    case "atomic_nand_relaxed":
      self = .init(name: .llvm(.atomic_nand_relaxed(t)))
    case "atomic_nand_acquire":
      self = .init(name: .llvm(.atomic_nand_acquire(t)))
    case "atomic_nand_release":
      self = .init(name: .llvm(.atomic_nand_release(t)))
    case "atomic_nand_acqrel":
      self = .init(name: .llvm(.atomic_nand_acqrel(t)))
    case "atomic_nand_seqcst":
      self = .init(name: .llvm(.atomic_nand_seqcst(t)))
    case "atomic_or_relaxed":
      self = .init(name: .llvm(.atomic_or_relaxed(t)))
    case "atomic_or_acquire":
      self = .init(name: .llvm(.atomic_or_acquire(t)))
    case "atomic_or_release":
      self = .init(name: .llvm(.atomic_or_release(t)))
    case "atomic_or_acqrel":
      self = .init(name: .llvm(.atomic_or_acqrel(t)))
    case "atomic_or_seqcst":
      self = .init(name: .llvm(.atomic_or_seqcst(t)))
    case "atomic_xor_relaxed":
      self = .init(name: .llvm(.atomic_xor_relaxed(t)))
    case "atomic_xor_acquire":
      self = .init(name: .llvm(.atomic_xor_acquire(t)))
    case "atomic_xor_release":
      self = .init(name: .llvm(.atomic_xor_release(t)))
    case "atomic_xor_acqrel":
      self = .init(name: .llvm(.atomic_xor_acqrel(t)))
    case "atomic_xor_seqcst":
      self = .init(name: .llvm(.atomic_xor_seqcst(t)))
    case "atomic_cmpxchg_relaxed_relaxed":
      self = .init(name: .llvm(.atomic_cmpxchg_relaxed_relaxed(t)))
    case "atomic_cmpxchg_relaxed_acquire":
      self = .init(name: .llvm(.atomic_cmpxchg_relaxed_acquire(t)))
    case "atomic_cmpxchg_relaxed_seqcst":
      self = .init(name: .llvm(.atomic_cmpxchg_relaxed_seqcst(t)))
    case "atomic_cmpxchg_acquire_relaxed":
      self = .init(name: .llvm(.atomic_cmpxchg_acquire_relaxed(t)))
    case "atomic_cmpxchg_acquire_acquire":
      self = .init(name: .llvm(.atomic_cmpxchg_acquire_acquire(t)))
    case "atomic_cmpxchg_acquire_seqcst":
      self = .init(name: .llvm(.atomic_cmpxchg_acquire_seqcst(t)))
    case "atomic_cmpxchg_release_relaxed":
      self = .init(name: .llvm(.atomic_cmpxchg_release_relaxed(t)))
    case "atomic_cmpxchg_release_acquire":
      self = .init(name: .llvm(.atomic_cmpxchg_release_acquire(t)))
    case "atomic_cmpxchg_release_seqcst":
      self = .init(name: .llvm(.atomic_cmpxchg_release_seqcst(t)))
    case "atomic_cmpxchg_acqrel_relaxed":
      self = .init(name: .llvm(.atomic_cmpxchg_acqrel_relaxed(t)))
    case "atomic_cmpxchg_acqrel_acquire":
      self = .init(name: .llvm(.atomic_cmpxchg_acqrel_acquire(t)))
    case "atomic_cmpxchg_acqrel_seqcst":
      self = .init(name: .llvm(.atomic_cmpxchg_acqrel_seqcst(t)))
    case "atomic_cmpxchg_seqcst_relaxed":
      self = .init(name: .llvm(.atomic_cmpxchg_seqcst_relaxed(t)))
    case "atomic_cmpxchg_seqcst_acquire":
      self = .init(name: .llvm(.atomic_cmpxchg_seqcst_acquire(t)))
    case "atomic_cmpxchg_seqcst_seqcst":
      self = .init(name: .llvm(.atomic_cmpxchg_seqcst_seqcst(t)))
    case "atomic_cmpxchgweak_relaxed_relaxed":
      self = .init(name: .llvm(.atomic_cmpxchgweak_relaxed_relaxed(t)))
    case "atomic_cmpxchgweak_relaxed_acquire":
      self = .init(name: .llvm(.atomic_cmpxchgweak_relaxed_acquire(t)))
    case "atomic_cmpxchgweak_relaxed_seqcst":
      self = .init(name: .llvm(.atomic_cmpxchgweak_relaxed_seqcst(t)))
    case "atomic_cmpxchgweak_acquire_relaxed":
      self = .init(name: .llvm(.atomic_cmpxchgweak_acquire_relaxed(t)))
    case "atomic_cmpxchgweak_acquire_acquire":
      self = .init(name: .llvm(.atomic_cmpxchgweak_acquire_acquire(t)))
    case "atomic_cmpxchgweak_acquire_seqcst":
      self = .init(name: .llvm(.atomic_cmpxchgweak_acquire_seqcst(t)))
    case "atomic_cmpxchgweak_release_relaxed":
      self = .init(name: .llvm(.atomic_cmpxchgweak_release_relaxed(t)))
    case "atomic_cmpxchgweak_release_acquire":
      self = .init(name: .llvm(.atomic_cmpxchgweak_release_acquire(t)))
    case "atomic_cmpxchgweak_release_seqcst":
      self = .init(name: .llvm(.atomic_cmpxchgweak_release_seqcst(t)))
    case "atomic_cmpxchgweak_acqrel_relaxed":
      self = .init(name: .llvm(.atomic_cmpxchgweak_acqrel_relaxed(t)))
    case "atomic_cmpxchgweak_acqrel_acquire":
      self = .init(name: .llvm(.atomic_cmpxchgweak_acqrel_acquire(t)))
    case "atomic_cmpxchgweak_acqrel_seqcst":
      self = .init(name: .llvm(.atomic_cmpxchgweak_acqrel_seqcst(t)))
    case "atomic_cmpxchgweak_seqcst_relaxed":
      self = .init(name: .llvm(.atomic_cmpxchgweak_seqcst_relaxed(t)))
    case "atomic_cmpxchgweak_seqcst_acquire":
      self = .init(name: .llvm(.atomic_cmpxchgweak_seqcst_acquire(t)))
    case "atomic_cmpxchgweak_seqcst_seqcst":
      self = .init(name: .llvm(.atomic_cmpxchgweak_seqcst_seqcst(t)))
    default:
      return nil
    }
  }

}

extension NativeInstruction.MathFlags {

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
private func mathFlags(_ stream: inout ArraySlice<Substring>) -> NativeInstruction.MathFlags {
  var result: NativeInstruction.MathFlags = []
  while let x = stream.first {
    guard let y = NativeInstruction.MathFlags(description: x) else { break }
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
