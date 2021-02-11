extension Diagnostic {

  public static func duplicateDeclaration(
    symbol name: String, range: SourceRange
  ) -> Diagnostic {
    return Diagnostic("duplicate declaration of '\(name)'", anchor: range)
  }

  public static func cannotFind(
    symbol name: String, range: SourceRange
  ) -> Diagnostic {
    return Diagnostic("I can't find '\(name)' in scope", anchor: range)
  }

  public static func cannotFind(
    type name: String, range: SourceRange
  ) -> Diagnostic {
    return Diagnostic("I can't find a type named '\(name)' in scope", anchor: range)
  }

  public static func cannotFind(
    type name: String, in type: Any, range: SourceRange
  ) -> Diagnostic {
    return Diagnostic("type '\(type)' has no member type named '\(name)' in scope", anchor: range)
  }

  public static func cannotFind(
    member: String, in type: Any, range: SourceRange
  ) -> Diagnostic {
    return Diagnostic("type '\(type)' has no member '\(member)'", anchor: range)
  }

  public static func cannotFind(
    builtin name: String, range: SourceRange
  ) -> Diagnostic {
    return Diagnostic("'\(name)' is not a built-in symbol", anchor: range)
  }

  public static func builtinTypesAreNotNamespaces(
    range: SourceRange
  ) -> Diagnostic {
    return Diagnostic("built-in types are not namespaces", anchor: range)
  }

  public static func nonNominalExtension(
    _ type: Any, range: SourceRange
  ) -> Diagnostic {
    return Diagnostic("I can't extend non-nominal type '\(type)'", anchor: range)
  }

  public static func missingPatternInitializer(
    range: SourceRange
  ) -> Diagnostic {
    return Diagnostic("unannotated pattern requires an initializer", anchor: range)
  }

  public static func wrongTuplePatternLength(
    type: Any, range: SourceRange
  ) -> Diagnostic {
    return Diagnostic("tuple pattern has the wrong lenght for type '\(type)'", anchor: range)
  }

  public static func ambiguousReference(
    to name: String, range: SourceRange
  ) -> Diagnostic {
    return Diagnostic("ambiguous reference to '\(name)'", anchor: range)
  }

  public static func callToNonFunction(
    type: Any, range: SourceRange
  ) -> Diagnostic {
    return Diagnostic("call to non-function type '\(type)'", anchor: range)
  }

  public static func missingReturnValue(
    range: SourceRange
  ) -> Diagnostic {
    return Diagnostic("non-unit function should return a value", anchor: range)
  }

  public static func conflictingEqualityRequitement(
    range: SourceRange
  ) -> Diagnostic {
    return Diagnostic("conflicting equality requirement", anchor: range)
  }

  public static func recursiveEqualityRequirement(
    range: SourceRange
  ) -> Diagnostic {
    return Diagnostic("recursive equality requirement", anchor: range)
  }

  public static func illegalConformanceRequirement(
    type: Any, range: SourceRange
  ) -> Diagnostic {
    return Diagnostic(
      "view conformance requirement on non-generic type '\(type)'", anchor: range)
  }

  public static func invalidConformanceRequirement(
    type: Any, range: SourceRange
  ) -> Diagnostic {
    return Diagnostic(
      "view conformance requirement to non-view type '\(type)'", anchor: range)
  }

  public static func referenceToGenericRequiresArguments(
    type: Any, range: SourceRange
  ) -> Diagnostic {
    return Diagnostic(
      "reference to generic type '\(type)' requires type arguments", anchor: range)
  }

  public static func tooManyGenericArguments(
    type: Any, got: Int, expected: Int, range: SourceRange
  ) -> Diagnostic {
    return Diagnostic(
      "type '\(type)' specialized with too many generic arguments " +
        "(got \(got), expected \(expected))",
      anchor: range)
  }

  public static func codeAfterReturnNeverExecuted(
    range: SourceRange
  ) -> Diagnostic {
    return Diagnostic(
      "code after return statement will never be executed",
      level: .warning,
      anchor: range)
  }

}
