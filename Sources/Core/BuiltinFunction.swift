import LLVM
import Utils

/// A function representing an IR instruction in Val source code.
public struct BuiltinFunction: Hashable {

  /// The name of the function.
  public let name: Name

  /// Returns the type of the function.
  public func type() -> LambdaType {
    switch self.name {
    case .addressOf:
      let p = ParameterType(.let, ^TypeVariable())
      return .init(inputs: [.init(label: "of", type: ^p)], output: .builtin(.ptr))

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
    /// Returns a pointer to a value passed as a `let` or `inout` argument. The returned pointer
    /// is only valid within the function in which `address_of` is being called.
    case addressOf

  }

}

extension BuiltinFunction.Name: CustomStringConvertible {

  public var description: String {
    switch self {
    case .llvm(let n):
      return n.description
    case .addressOf:
      return "address_of(_:)"
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
    default:
      self.init(native: n)
    }
  }

  /// Creates a built-in function representing the native instruction nameed `n` or returns `nil`
  /// if `n` isn't a valid native instruction name.
  private init?(native n: String) {
    var tokens = n.split(separator: "_")[...]

    // The first token is the LLVM instruction name.
    guard let instruction = tokens.popFirst().map(String.init(_:)) else { return nil }
    switch instruction {
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

    case "zeroinitializer":
      guard let t = builtinType(&tokens) else { return nil }
      self = .init(name: .llvm(.zeroinitializer(t)))

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

/// Returns a parser that returns the result of applying `a` and then `b` or `nil` if either `a`
/// or `b` returns `nil`.
private func ++ <A, B>(_ a: @escaping Parser<A>, _ b: @escaping Parser<B>) -> Parser<(A, B)> {
  { (stream: inout ArraySlice<Substring>) -> (A, B)? in
    a(&stream).flatMap({ (x) in b(&stream).map({ (x, $0) }) })
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

/// Returns an overlflow behavior parsed from `stream` or `.ignore` if none can be parsed.
private func overflowBehavior(_ stream: inout ArraySlice<Substring>) -> LLVM.OverflowBehavior {
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

/// Parses the parameters and type of an integer arithmetic function instruction.
private let integerArithmeticTail =
  overflowBehavior ++ builtinType

/// Parses the parameters and type of `icmp`.
private let integerComparisonTail =
  take(LLVM.IntegerPredicate.self) ++ builtinType

/// Parses the parameters and type of a floating-point arithmetic instruction.
private let floatingPointArithmeticTail =
  mathFlags ++ builtinType

/// Parses the parameters and type of `fcmp`.
private let floatingPointComparisonTail =
  mathFlags ++ take(LLVM.FloatingPointPredicate.self) ++ builtinType
