extension Diag {

  static func cannotDestructure(_ type: ValType, range: SourceRange?) -> Diag {
    return Diag("cannot destructure value of type '\(type)'", anchor: range)
  }

  static func cannotSpecializeNonGenericType(_ type: ValType, range: SourceRange?) -> Diag {
    return Diag("cannot specialize non-generic type '\(type)'", anchor: range)
  }

  static func duplicateTupleLabel(range: SourceRange?) -> Diag {
    return Diag("duplicate tuple label", anchor: range)
  }

  static func incompatibleTupleLabel(range: SourceRange?) -> Diag {
    return Diag("incompatible tuple label", anchor: range)
  }

  static func incompatibleTupleLengths(range: SourceRange?) -> Diag {
    return Diag("incompatible tuple lengths", anchor: range)
  }

  static func missingReturnValue(range: SourceRange?) -> Diag {
    return Diag("missing return value", anchor: range)
  }

  static func missingTypeAnnotation(range: SourceRange?) -> Diag {
    return Diag("mssing type annotation", anchor: range)
  }

  static func nonViewTypeRequirement(on type: ValType, range: SourceRange?) -> Diag {
    return Diag("illegal conformance requirement on non-view type '\(type)'", anchor: range)
  }

  static func notInEnvironment(_ type: ValType, range: SourceRange?) -> Diag {
    return Diag(
      "type '\(type)' does not refer to a type parameter or associated in this environment",
      anchor: range)
  }

  static func tooManyTypeArguments(expected: Int, got: Int, range: SourceRange?) -> Diag {
    return Diag("too many type arguments (expected \(expected), got \(got))", anchor: range)
  }

  static func typeRequiresArguments(_ type: ValType, range: SourceRange?) -> Diag {
    return Diag("type '\(type)' requires type arguments", anchor: range)
  }

}
