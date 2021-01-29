extension Diagnostic {

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

  public static func ambiguousReference(
    to name: String, range: SourceRange
  ) -> Diagnostic {
    return Diagnostic("ambiguous reference to '\(name)'", anchor: range)
  }

  public static func callToNonFunction(
    type: Any, range: SourceRange
  ) -> Diagnostic {
    return Diagnostic("I can't call non-function type '\(type)'", anchor: range)
  }

  public static func nonNominalExtension(
    _ type: Any, range: SourceRange
  ) -> Diagnostic {
    return Diagnostic("I can't extend non-nominal type '\(type)'", anchor: range)
  }

}
