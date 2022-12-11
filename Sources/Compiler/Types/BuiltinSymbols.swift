/// A namespace containing the types of the built-in functions and subscripts.
public enum BuiltinSymbols {

  /// Terminates the program abnormally.
  public static let terminate = LambdaType(to: .never)

  /// 1-bit integer copy.
  public static let i1_copy = LambdaType(from: (.let, .i(1)), to: .builtin(.i(1)))

  /// 32-bit integer copy.
  public static let i32_copy = LambdaType(from: (.let, .i(32)), to: .builtin(.i(32)))

  /// 64-bit integer copy.
  public static let i64_copy = LambdaType(from: (.let, .i(64)), to: .builtin(.i(64)))

  /// 64-bit integer multiplication.
  public static let i64_mul = LambdaType(from: (.let, .i(64)), (.let, .i(64)), to: .builtin(.i(64)))

  /// 64-bit integer addition.
  public static let i64_add = LambdaType(from: (.let, .i(64)), (.let, .i(64)), to: .builtin(.i(64)))

  /// 64-bit integer subtraction.
  public static let i64_sub = LambdaType(from: (.let, .i(64)), (.let, .i(64)), to: .builtin(.i(64)))

  /// 64-bit integer "less than" comparison.
  public static let i64_lt = LambdaType(from: (.let, .i(64)), (.let, .i(64)), to: .builtin(.i(1)))

  // 64-bit integer unchecked conversion to 32-bit.
  public static let i64_trunc_to_i32 = LambdaType(from: (.let, .i(64)), to: .builtin(.i(32)))

  // 64-bit print.
  public static let i64_print = LambdaType(from: (.let, .i(64)), to: .void)

  // Double-precision floating-point copy.
  public static let f64_copy = LambdaType(from: (.let, .f64), to: .builtin(.f64))

  /// Returns the type of the built-in function with the given name.
  public static subscript(_ name: String) -> LambdaType? {
    switch name {
    case "terminate": return Self.terminate

    case "i1_copy": return Self.i1_copy

    case "i32_copy": return Self.i32_copy

    case "i64_copy": return Self.i64_copy
    case "i64_mul": return Self.i64_mul
    case "i64_add": return Self.i64_add
    case "i64_sub": return Self.i64_sub
    case "i64_lt": return Self.i64_lt
    case "i64_trunc_to_i32": return Self.i64_trunc_to_i32
    case "i64_print": return Self.i64_print

    case "f64_copy": return Self.f64_copy

    default:
      return nil
    }
  }

}

extension LambdaType {

  fileprivate init(from inputs: (PassingConvention, BuiltinType)..., to output: AnyType) {
    self.init(
      inputs: inputs.map({ (convention, type) -> CallableTypeParameter in
        .init(type: ^ParameterType(convention: convention, bareType: .builtin(type)))
      }), output: output)
  }

}
