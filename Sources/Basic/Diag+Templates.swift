extension Diag {

  public static func multipleStatementInMatchExpression(
    range: SourceRange?
  ) -> Diag {
    return Diag(
      "case of match used as an expression must be a single expression",
      anchor: range)
  }

  public static func duplicateDeclaration(
    symbol name: String, range: SourceRange?
  ) -> Diag {
    return Diag("duplicate declaration of '\(name)'", anchor: range)
  }

  public static func cannotFind(
    symbol name: String, range: SourceRange?
  ) -> Diag {
    return Diag("I can't find '\(name)' in scope", anchor: range)
  }

  public static func cannotFind(
    type name: String, range: SourceRange?
  ) -> Diag {
    return Diag("I can't find a type named '\(name)' in scope", anchor: range)
  }

  public static func cannotFind(
    type name: String, in type: Any, range: SourceRange?
  ) -> Diag {
    return Diag("type '\(type)' has no member type named '\(name)'", anchor: range)
  }

  public static func cannotFind(
    member: String, in type: Any, range: SourceRange?
  ) -> Diag {
    return Diag("type '\(type)' has no member '\(member)'", anchor: range)
  }

  public static func cannotFind(
    builtin name: String, range: SourceRange?
  ) -> Diag {
    return Diag("'\(name)' is not a built-in symbol", anchor: range)
  }

  public static func cannotFind(
    module name: String, range: SourceRange?
  ) -> Diag {
    return Diag("I don't know about a module named '\(name)'", anchor: range)
  }

  public static func cannotReferToForeignTypeParameter(
    _ parameter: String, range: SourceRange?
  ) -> Diag {
    return Diag(
      "cannot refer to generic type parameter: '\(parameter)' is defined in another scope",
      anchor: range)
  }

  public static func builtinTypesAreNotNamespaces(
    range: SourceRange?
  ) -> Diag {
    return Diag("built-in types are not namespaces", anchor: range)
  }

  public static func nonViewTypeInViewComposition(
    type name: Any, range: SourceRange?
  ) -> Diag {
    return Diag("cannot insert non-view type '\(name)' into view composition", anchor: range)
  }

  public static func nonNominalExtension(
    _ type: Any, range: SourceRange?
  ) -> Diag {
    return Diag("I can't extend non-nominal type '\(type)'", anchor: range)
  }

  public static func typeAliasReferencesItself(
    type name: String, range: SourceRange?
  ) -> Diag {
    return Diag("type alias '\(name)' references itself", anchor: range)
  }

  public static func newConformanceOnNominalTypeAlias(
    range: SourceRange?
  ) -> Diag {
    return Diag(
      """
      I can't declare new conformances for a nominal type through an alias declaration; \
      use an extension
      """,
      anchor: range)
  }

  public static func invalidMutatingTypeModifier(
    range: SourceRange?
  ) -> Diag {
    return Diag("mutating type modifier is only valid on function parameters", anchor: range)
  }

  public static func missingPatternInitializer(
    range: SourceRange?
  ) -> Diag {
    return Diag("unannotated pattern requires an initializer", anchor: range)
  }

  public static func wrongTuplePatternLength(
    type: Any, range: SourceRange?
  ) -> Diag {
    return Diag("tuple pattern has the wrong lenght for type '\(type)'", anchor: range)
  }

  public static func ambiguousReference(
    to name: String, range: SourceRange?
  ) -> Diag {
    return Diag("ambiguous reference to '\(name)'", anchor: range)
  }

  public static func callToNonFunction(
    type: Any, range: SourceRange?
  ) -> Diag {
    return Diag("call to non-function type '\(type)'", anchor: range)
  }

  public static func missingReturnValue(
    range: SourceRange?
  ) -> Diag {
    return Diag("non-unit function should return a value", anchor: range)
  }

  public static func conflictingEqualityRequirement(
    range: SourceRange?
  ) -> Diag {
    return Diag("conflicting equality requirement", anchor: range)
  }

  public static func recursiveEqualityRequirement(
    range: SourceRange?
  ) -> Diag {
    return Diag("recursive equality requirement", anchor: range)
  }

  public static func illegalConformanceRequirement(
    type: Any, range: SourceRange?
  ) -> Diag {
    return Diag(
      "view conformance requirement on non-generic type '\(type)'", anchor: range)
  }

  public static func nonViewTypeConformanceRequirement(
    type: Any, range: SourceRange?
  ) -> Diag {
    return Diag(
      "view conformance requirement to non-view type '\(type)'", anchor: range)
  }

  public static func conformanceRequiresMatchingImplementation(
    view: String, requirement: String, range: SourceRange?
  ) -> Diag {
    return Diag(
      "conformance to '\(view)' requires a matching implementation of '\(requirement)'",
      anchor: range)
  }

  public static func cannotSpecializeNonGenericType(
    type: Any, range: SourceRange?
  ) -> Diag {
    return Diag("cannot specialize non-generic type \(type)", anchor: range)
  }

  public static func referenceToGenericTypeRequiresArguments(
    type: Any, range: SourceRange?
  ) -> Diag {
    return Diag(
      "reference to generic type '\(type)' requires type arguments", anchor: range)
  }

  public static func tooManyGenericArguments(
    type: Any, got: Int, expected: Int, range: SourceRange?
  ) -> Diag {
    return Diag(
      """
      type '\(type)' specialized with too many generic arguments: \
      (got \(got), expected \(expected))
      """,
      anchor: range)
  }

  public static func cannotAssignToImmutableLocation(
    range: SourceRange?
  ) -> Diag {
    return Diag("I can't assign an immutable location", anchor: range)
  }

  public static func cannotAssignImmutableSelf(
    range: SourceRange?
  ) -> Diag {
    return Diag("I can't assign property to of immutable 'self'", anchor: range)
  }

  public static func cannotAssignImmutableExpr(
    range: SourceRange?
  ) -> Diag {
    return Diag("I can't assign an immutable expression", anchor: range)
  }

  public static func missingReturnValueInNonUnitFunction(
    range: SourceRange?
  ) -> Diag {
    return Diag("missing return value in non-unit function", anchor: range)
  }

  public static func superfluousTypeModifier(
    range: SourceRange?
  ) -> Diag {
    return Diag("superfluous type qualified", level: .warning, anchor: range)
  }

  public static func codeAfterReturnNeverExecuted(
    range: SourceRange?
  ) -> Diag {
    return Diag(
      "code after return statement will never be executed",
      level: .warning,
      anchor: range)
  }

  public static func unsafeCastToSameTypeHasNoEffect(
    type: Any, range: SourceRange?
  ) -> Diag {
    return Diag(
      "unsafe cast from '\(type)' to the same type has no effect",
      level: .warning,
      anchor: range)
  }

}
