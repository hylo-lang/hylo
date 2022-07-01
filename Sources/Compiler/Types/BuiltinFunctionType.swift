/// A namespace containing the types of the built-in functions.
public enum BuiltinFunctionType {

  /// Uncondtionally stops the program.
  public static let terminate = LambdaType(to: .never)

  /// 64-bit integer copy.
  public static let i64_copy = LambdaType(
    from: (.let, .i(64)),
    to: .builtin(.i(64)))

  /// 64-bit integer addition.
  public static let i64_add = LambdaType(
    from: (.sink, .i(64)),
    to: .builtin(.i(64)))

  /// Returns the type of the built-in function with the given name.
  public static subscript(_ name: String) -> LambdaType? {
    switch name {
    case "terminate": return Self.terminate
    case "i64_copy" : return Self.i64_copy
    case "i64_add"  : return Self.i64_add
    default:
      return nil
    }
  }

}

extension LambdaType {

  fileprivate init(from inputs: (ParamConvention, BuiltinType)..., to output: Type) {
    self.init(
      inputs: inputs.map({ (convention, type) in
        CallableTypeParameter(
          type: .parameter(ParameterType(convention: convention, bareType: .builtin(type))))
      }),
      output: output)
  }

}
