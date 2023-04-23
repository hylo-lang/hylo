import Utils

/// A function representing an IR instruction in Val source code.
public struct BuiltinFunction: Hashable {

  /// The name of the function.
  public let name: Name

  /// The type of the function.
  public let type: LambdaType

}

extension BuiltinFunction {
  
  /// The name of a built-in function.
  public enum Name: Hashable {
    
    /// An LLVM instruction.
    case llvm(NativeInstruction)
    
  }
  
}

extension BuiltinFunction.Name: CustomStringConvertible {

  public var description: String {
    switch self {
    case .llvm(let n):
      return n.description
    }
  }

}

// MARK: Parsing

extension BuiltinFunction {

  /// Creates an instance by parsing its name from `s`, returning `nil` if `s` isn't a valid
  /// function name.
  public init?(_ s: String) {
    var tokens = s.split(separator: "_")[...]

    // The first token is the LLVM instruction name.
    guard let instruction = tokens.popFirst().map(String.init(_:)) else { return nil }
    switch instruction {
    case "add":
      guard let (p, t) = integerArithmeticTail(&tokens) else { return nil }
      self = .init(name: .llvm(.add(p, t)), type: .init(^t, ^t, to: ^t))

    case "sub":
      guard let (p, t) = integerArithmeticTail(&tokens) else { return nil }
      self = .init(name: .llvm(.sub(p, t)), type: .init(^t, ^t, to: ^t))

    case "mul":
      guard let (p, t) = integerArithmeticTail(&tokens) else { return nil }
      self = .init(name: .llvm(.mul(p, t)), type: .init(^t, ^t, to: ^t))

    case "shl":
      guard let (p, t) = integerArithmeticTail(&tokens) else { return nil }
      self = .init(name: .llvm(.shl(p, t)), type: .init(^t, ^t, to: ^t))

    case "udiv":
      guard let (p, t) = (maybe("exact") ++ builtinType)(&tokens) else { return nil }
      self = .init(name: .llvm(.udiv(exact: p != nil, t)), type: .init(^t, ^t, to: ^t))

    case "sdiv":
      guard let (p, t) = (maybe("exact") ++ builtinType)(&tokens) else { return nil }
      self = .init(name: .llvm(.sdiv(exact: p != nil, t)), type: .init(^t, ^t, to: ^t))

    case "lshr":
      guard let (p, t) = (maybe("exact") ++ builtinType)(&tokens) else { return nil }
      self = .init(name: .llvm(.lshr(exact: p != nil, t)), type: .init(^t, ^t, to: ^t))

    case "ashr":
      guard let (p, t) = (maybe("exact") ++ builtinType)(&tokens) else { return nil }
      self = .init(name: .llvm(.ashr(exact: p != nil, t)), type: .init(^t, ^t, to: ^t))

    case "urem":
      guard let t = builtinType(&tokens) else { return nil }
      self = .init(name: .llvm(.urem(t)), type: .init(^t, ^t, to: ^t))

    case "srem":
      guard let t = builtinType(&tokens) else { return nil }
      self = .init(name: .llvm(.srem(t)), type: .init(^t, ^t, to: ^t))

    case "and":
      guard let t = builtinType(&tokens) else { return nil }
      self = .init(name: .llvm(.and(t)), type: .init(^t, ^t, to: ^t))

    case "or":
      guard let t = builtinType(&tokens) else { return nil }
      self = .init(name: .llvm(.or(t)), type: .init(^t, ^t, to: ^t))

    case "xor":
      guard let t = builtinType(&tokens) else { return nil }
      self = .init(name: .llvm(.xor(t)), type: .init(^t, ^t, to: ^t))

    case "icmp":
      guard let (p, t) = integerComparisonTail(&tokens) else { return nil }
      self = .init(name: .llvm(.icmp(p, t)), type: .init(^t, ^t, to: .builtin(.i(1))))

    case "trunc":
      guard let (s, d) = (builtinType ++ builtinType)(&tokens) else { return nil }
      self = .init(name: .llvm(.trunc(s, d)), type: .init(^s, to: ^d))

    case "zext":
      guard let (s, d) = (builtinType ++ builtinType)(&tokens) else { return nil }
      self = .init(name: .llvm(.zext(s, d)), type: .init(^s, to: ^d))

    case "sext":
      guard let (s, d) = (builtinType ++ builtinType)(&tokens) else { return nil }
      self = .init(name: .llvm(.sext(s, d)), type: .init(^s, to: ^d))

    case "uitofp":
      guard let (s, d) = (builtinType ++ builtinType)(&tokens) else { return nil }
      self = .init(name: .llvm(.uitofp(s, d)), type: .init(^s, to: ^d))

    case "sitofp":
      guard let (s, d) = (builtinType ++ builtinType)(&tokens) else { return nil }
      self = .init(name: .llvm(.sitofp(s, d)), type: .init(^s, to: ^d))

    case "fadd":
      guard let (p, t) = floatingPointArithmeticTail(&tokens) else { return nil }
      self = .init(name: .llvm(.fadd(p, t)), type: .init(^t, ^t, to: ^t))

    case "fsub":
      guard let (p, t) = floatingPointArithmeticTail(&tokens) else { return nil }
      self = .init(name: .llvm(.fsub(p, t)), type: .init(^t, ^t, to: ^t))

    case "fmul":
      guard let (p, t) = floatingPointArithmeticTail(&tokens) else { return nil }
      self = .init(name: .llvm(.fmul(p, t)), type: .init(^t, ^t, to: ^t))

    case "fdiv":
      guard let (p, t) = floatingPointArithmeticTail(&tokens) else { return nil }
      self = .init(name: .llvm(.fdiv(p, t)), type: .init(^t, ^t, to: ^t))

    case "frem":
      guard let (p, t) = floatingPointArithmeticTail(&tokens) else { return nil }
      self = .init(name: .llvm(.frem(p, t)), type: .init(^t, ^t, to: ^t))

    case "fcmp":
      guard let (p, t) = floatingPointComparisonTail(&tokens) else { return nil }
      self = .init(name: .llvm(.fcmp(p.0, p.1, t)), type: .init(^t, ^t, to: .builtin(.i(1))))

    case "fptrunc":
      guard let (s, d) = (builtinType ++ builtinType)(&tokens) else { return nil }
      self = .init(name: .llvm(.fptrunc(s, d)), type: .init(^s, to: ^d))

    case "fpext":
      guard let (s, d) = (builtinType ++ builtinType)(&tokens) else { return nil }
      self = .init(name: .llvm(.fpext(s, d)), type: .init(^s, to: ^d))

    case "fptoui":
      guard let (s, d) = (builtinType ++ builtinType)(&tokens) else { return nil }
      self = .init(name: .llvm(.fptoui(s, d)), type: .init(^s, to: ^d))

    case "fptosi":
      guard let (s, d) = (builtinType ++ builtinType)(&tokens) else { return nil }
      self = .init(name: .llvm(.fptosi(s, d)), type: .init(^s, to: ^d))

    case "zeroinitializer":
      guard let t = builtinType(&tokens) else { return nil }
      self = .init(name: .llvm(.zeroinitializer(t)), type: .init(to: ^t))

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

/// Returns a parser that returns `.some(r)` where `r` is the result of `a`, guaranteeing `stream`
/// is left unchanged if `a` returns `nil`.
private func maybe<T>(_ a: @escaping Parser<T>) -> Parser<T?> {
  { (stream: inout ArraySlice<Substring>) -> T?? in
    let s = stream
    if let r = a(&stream) {
      return .some(r)
    } else {
      stream = s
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

/// Returns a parser that returns an elements in `choices` if an equal value can be consumed.
private func take<T: RawRepresentable>(_: T.Type) -> Parser<T> where T.RawValue == String {
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

/// Parses the parameters and type of an integer arithmetic function instruction.
private let integerArithmeticTail =
  maybe(take(NativeInstruction.IntegerArithmeticParameter.self)) ++ builtinType

/// Parses the parameters and type of `icmp`.
private let integerComparisonTail =
  take(NativeInstruction.IntegerPredicate.self) ++ builtinType

/// Parses the parameters and type of a floating-point arithmetic instruction.
private let floatingPointArithmeticTail =
  mathFlags ++ builtinType

/// Parses the parameters and type of `fcmp`.
private let floatingPointComparisonTail =
  mathFlags ++ take(NativeInstruction.FloatingPointPredicate.self) ++ builtinType
