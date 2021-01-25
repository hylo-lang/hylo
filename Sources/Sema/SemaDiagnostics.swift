import AST
import Basic

extension Diagnostic {

  static func cannotFind(name: String, range: SourceRange) -> Diagnostic {
    return Diagnostic("I can't find '\(name)' in scope")
      .set(\.reportLocation, value: range.lowerBound)
      .set(\.ranges, value: [range])
  }

  static func cannotFind(bultinName name: String, range: SourceRange) -> Diagnostic {
    return Diagnostic("I can't find '\(name)' in the built-in module")
      .set(\.reportLocation, value: range.lowerBound)
      .set(\.ranges, value: [range])
  }

  static func cannotFind(name: String, in type: ValType, range: SourceRange) -> Diagnostic {
    return Diagnostic("type '\(type)' has no member '\(name)'")
      .set(\.reportLocation, value: range.lowerBound)
      .set(\.ranges, value: [range])
  }

  static func cannotFind(typeName name: String, range: SourceRange) -> Diagnostic {
    return Diagnostic("I can't find a type named '\(name)' in scope")
      .set(\.reportLocation, value: range.lowerBound)
      .set(\.ranges, value: [range])
  }

  static func cannotFind(
    typeName name: String, in type: ValType, range: SourceRange
  ) -> Diagnostic {
    return Diagnostic("type '\(type)' has no member type named '\(name)' in scope")
      .set(\.reportLocation, value: range.lowerBound)
      .set(\.ranges, value: [range])
  }

  static func builtinTypesAreNotNamespaces(range: SourceRange) -> Diagnostic {
    return Diagnostic("built-in types are not namespaces")
      .set(\.reportLocation, value: range.lowerBound)
      .set(\.ranges, value: [range])
  }

  static func cannotCallNonFunctionType(type: ValType, range: SourceRange) -> Diagnostic {
    return Diagnostic("I can't call non-function type '\(type)'")
      .set(\.reportLocation, value: range.lowerBound)
      .set(\.ranges, value: [range])
  }

  static func ambiguousReference(to name: String, range: SourceRange) -> Diagnostic {
    return Diagnostic("ambiguous reference to '\(name)'")
      .set(\.reportLocation, value: range.lowerBound)
      .set(\.ranges, value: [range])
  }

  static func cannotExtendNonNominalType(_ type: ValType, range: SourceRange) -> Diagnostic {
    return Diagnostic("I can't extend non-nominal type '\(type)'")
      .set(\.reportLocation, value: range.lowerBound)
      .set(\.ranges, value: [range])
  }

}
