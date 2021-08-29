import VIL

/// A "thick" function, pairing a (potentially thick) function with a pointer to its environment.
struct ThickFunction {

  /// A value representing the delegator of a partially applied function.
  enum Delegator {

    case thin(UnsafeRawPointer)

    case thick(UnsafeRawPointer)

  }

  /// A buffer representing the function's environment.
  ///
  /// The buffer starts with a pair `(count, align)` of 32-bit unsigned integers denoting the
  /// number of captures and their memory alignment `align`, respectively. After that header, there
  /// is a sequence of `count` pointers to the value witness table of each capture. The captures
  /// are laid out after these pointers, represented as an instance of a compound type.
  let env: UnsafeRawPointer?

  /// The function to which the call is forwarded.
  let delegator: Delegator

  init(delegator: Delegator, env: UnsafeRawPointer?) {
    self.delegator = delegator
    self.env = env
  }

  init(thin function: VIL.Function) {
    self.delegator = .thin(UnsafeRawPointer(Unmanaged.passUnretained(function).toOpaque()))
    self.env = nil
  }

}

/// A built-in function.
struct BuiltinFunction {

  enum ID: String {

    case set_status

    case i64_print
    case i64_trunc_IntLiteral
    case i64_add
    case i64_sub
    case i64_mul
    case i64_div
    case i64_rem
    case i64_neg
    case i64_abs

  }

  var id: ID

  init(id: ID) {
    self.id = id
  }

  init?(literal: BuiltinFunRef) {
    guard let id = ID(rawValue: literal.decl.name) else { return nil }
    self.init(id: id)
  }

  func apply(to args: [RuntimeValue], in interpreter: inout Interpreter) -> RuntimeValue {
    switch id {
    case .set_status:
      let a = args[0].open(as: Int.self)
      interpreter.status = a
      return .unit

    case .i64_print:
      let a = args[0].open(as: Int.self)
      interpreter.standardOutput.write("\(a)\n".data(using: .utf8)!)
      return .unit

    case .i64_trunc_IntLiteral:
      assert(args[0].byteCount == MemoryLayout<Int>.size)
      return args[0]

    case .i64_add:
      let a = args[0].open(as: Int.self)
      let b = args[1].open(as: Int.self)
      return RuntimeValue(copyingRawBytesOf: a + b)

    case .i64_sub:
      let a = args[0].open(as: Int.self)
      let b = args[1].open(as: Int.self)
      return RuntimeValue(copyingRawBytesOf: a - b)

    case .i64_mul:
      let a = args[0].open(as: Int.self)
      let b = args[1].open(as: Int.self)
      return RuntimeValue(copyingRawBytesOf: a * b)

    case .i64_div:
      let a = args[0].open(as: Int.self)
      let b = args[1].open(as: Int.self)
      return RuntimeValue(copyingRawBytesOf: a / b)

    case .i64_rem:
      let a = args[0].open(as: Int.self)
      let b = args[1].open(as: Int.self)
      return RuntimeValue(copyingRawBytesOf: a % b)

    case .i64_neg:
      let a = args[0].open(as: Int.self)
      return RuntimeValue(copyingRawBytesOf: -a)

    case .i64_abs:
      let a = args[0].open(as: Int.self)
      return RuntimeValue(copyingRawBytesOf: abs(a))
    }
  }

}
