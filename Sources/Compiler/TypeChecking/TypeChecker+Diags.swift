extension Diag {

  static func duplicateTupleLabel(range: SourceRange?) -> Diag {
    return Diag("duplicate tuple label", anchor: range)
  }

  static func cannotSpecializeNonGenericType(_ type: ValType, range: SourceRange?) -> Diag {
    return Diag("cannot specialize non-generic type \(type)", anchor: range)
  }

  static func tooManyTypeArguments(expected: Int, got: Int, range: SourceRange?) -> Diag {
    return Diag("too many type arguments (expected \(expected), got \(got))", anchor: range)
  }

}
