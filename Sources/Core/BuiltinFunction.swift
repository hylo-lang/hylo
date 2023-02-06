import Utils

/// A function representing an IR instruction in Val source code.
///
/// Built-in functions implement basis operations on built-in types, such as `Builtin.i64`, with
/// the same semantics as their corresponding LLVM instruction.
///
/// LLVM instructions are "generic" in the sense that they can typically be parameterized by types
/// and flags. For example, `add i32` and `add i64` represent the integer addition parameterized
/// for 32-bit and 64-bit integer values, respectively. In Val, this parameterization is encoded
/// directly into the name of a built-in function. For example, `Builtin.add_i64` corresponds to
/// LLVM's `add i64` instruction.
///
/// The name of a built-in function is given by the name of its corresponding LLVM instruction
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
/// Supported operations include all LLVM arithmetic and comparison instructions on built-in integral
/// and floating-point numbers as well as conversions from and to these types.
public struct BuiltinFunction: Hashable {

  /// The name of the notional generic function of which `self` is an instance.
  public let llvmInstruction: String

  /// The generic parameters of the function.
  public let genericParameters: [String]

  /// The type of the function.
  public let type: LambdaType

  /// Creates an instance with the given properties.
  private init(llvmInstruction: String, genericParameters: [String], type: LambdaType) {
    self.llvmInstruction = llvmInstruction
    self.genericParameters = genericParameters
    self.type = type
  }

  /// Creates an instance by parsing it from `s` or returns `nil` if `s` isn't a valid built-in
  /// function name.
  public init?(_ s: String) {
    let make = Self.init(llvmInstruction:genericParameters:type:)
    var tokens = s.split(separator: "_")[...]

    // The first token is the LLVM instruction name.
    guard let instruction = tokens.popFirst().map(String.init(_:)) else { return nil }
    switch instruction {
    case "copy":
      guard let t = builtinType(&tokens) else { return nil }
      self = make(instruction, [], LambdaType(^t, to: ^t))

    case "add", "sub", "mul", "shl":
      guard let (p, t) = integerArithmeticParameters(&tokens) else { return nil }
      self = make(instruction, [p.0, p.1].compactMap({ $0 }), LambdaType(^t, ^t, to: ^t))

    case "udiv", "sdiv", "lshr", "ashr":
      guard let (p, t) = (maybe("exact") ++ builtinType)(&tokens) else { return nil }
      self = make(instruction, p.map({ [$0] }) ?? [], LambdaType(^t, ^t, to: ^t))

    case "urem", "srem", "and", "or", "xor":
      guard let t = builtinType(&tokens) else { return nil }
      self = make(instruction, [], LambdaType(^t, ^t, to: ^t))

    case "icmp":
      guard let (p, t) = integerComparisonParameters(&tokens) else { return nil }
      self = make(instruction, [p], LambdaType(^t, ^t, to: .builtin(.i(1))))

    case "trunc", "zext", "sext", "uitofp", "sitofp":
      guard let (s, d) = (builtinType ++ builtinType)(&tokens) else { return nil }
      self = make(instruction, [], LambdaType(^s, to: ^d))

    case "fadd", "fsub", "fmul", "fdiv", "frem":
      guard let (p, t) = floatingPointArithmeticParameters(&tokens) else { return nil }
      self = make(instruction, p, LambdaType(^t, ^t, to: ^t))

    case "fcmp":
      guard let (p, t) = floatingPointComparisonParameters(&tokens) else { return nil }
      self = make(instruction, p.0 + [p.1], LambdaType(^t, ^t, to: .builtin(.i(1))))

    case "fptrunc", "fpext", "fptoui", "fptosi":
      guard let (s, d) = (builtinType ++ builtinType)(&tokens) else { return nil }
      self = make(instruction, [], LambdaType(^s, to: ^d))

    case "zeroinitializer":
      guard let t = builtinType(&tokens) else { return nil }
      self = make(instruction, [], LambdaType(to: ^t))

    default:
      return nil
    }
  }

}

/// A function that parses an instance of `T` by consuming a prefix of `tokens`, or returns `nil` 
/// if such instance cannot be parsed.
///
/// - Note: a prefix of `tokens` may have been consumed even if the function returns `nil`.
private typealias Parser<T> = (_ tokens: inout ArraySlice<Substring>) -> T?

/// Returns a parser that consumes an element equal to `s` and returns `.some(s)`, or returns
/// `.some(nil)` if such an element can't be consumed.
private func maybe(_ s: String) -> Parser<String?> {
  { (stream: inout ArraySlice<Substring>) -> String?? in
    if let t = stream.first, t == s {
      stream.removeFirst()
      return .some(s)
    } else {
      return .some(nil)
    }
  }
}

/// Returns a parser that returns the result of applying `a` and then `b` or `nil` if either `a`
/// or `b` returns `nil`.
private func ++ <A, B>(_ a: @escaping Parser<A>, _ b: @escaping Parser<B>) -> Parser<(A, B)> {
  { (stream: inout ArraySlice<Substring>) -> (A, B)? in
    a(&stream).flatMap({ (x) in b(&stream).map({ (x, $0) }) })
  }
}

/// Returns a parser that returns an elements in `choices` if an equal value can be consumed.
private func one(of choices: [String]) -> Parser<String> {
  { (stream: inout ArraySlice<Substring>) -> String? in
    stream.popFirst().flatMap({ (x) in choices.first(where: { $0 == x }) })
  }
}

/// Returns a built-in type parsed from `stream`.
private func builtinType(_ stream: inout ArraySlice<Substring>) -> BuiltinType? {
  stream.popFirst().flatMap(BuiltinType.init(_:))
}

/// Returns the longest sequence of floating-point math flags that can be parsed from `stream`.
private func mathFlags(_ stream: inout ArraySlice<Substring>) -> [String] {
  var result: [String] = []
  while let x = stream.first {
    guard
      let y = ["afn", "arcp", "contract", "fast", "ninf", "nnan", "nsz", "reassoc"]
        .first(where: { $0 == x })
    else { break }
    stream.removeFirst()
    result.append(y)
  }
  return result
}

/// Parses the generic parameters of an integer arithmetic function.
private let integerArithmeticParameters = maybe("nuw") ++ maybe("nsw") ++ builtinType

/// Parses the generic parameters of `icmp`.
private let integerComparisonParameters =
  one(of: ["eq", "ne", "ugt", "uge", "ult", "ule", "sgt", "sge", "slt", "sle"])
  ++ builtinType

/// Parses the generic parameters of an floating-point arithmetic function.
private let floatingPointArithmeticParameters = mathFlags ++ builtinType

/// Parses the generic parameters of `fcmp`.
private let floatingPointComparisonParameters =
  mathFlags
  ++ one(of: [
    "oeq", "ogt", "oge", "olt", "ole", "one", "ord", "ueq", "ugt", "uge", "ult", "ule", "une",
    "uno", "true", "false",
  ])
  ++ builtinType
