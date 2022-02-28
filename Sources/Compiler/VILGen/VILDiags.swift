extension Diag {

  // MARK: VILGen

  static func runtimeFunctionTypeConversion(range: SourceRange?) -> Diag {
    return Diag(
      .warning,
      "runtime conversion of function types is not supported; cast will always fail",
      anchor: range)
  }

  static func immutableBindingRequiresInitializer(decl: PatternBindingDecl) -> Diag {
    return Diag(
      .error,
      "local immutable binding requires an initializer",
      anchor: decl.range)
  }

  static func castAlwaysFails(
    from lhs: ValType,
    to rhs: ValType,
    range: SourceRange?
  ) -> Diag {
    return Diag(
      .warning,
      "conversion from '\(lhs)' to '\(rhs)' will always fail",
      anchor: range)
  }

  static func castAlwaysSucceeds(
    from lhs: ValType,
    to rhs: ValType,
    range: SourceRange?
  ) -> Diag {
    return Diag(
      .warning,
      "conversion from '\(lhs)' to '\(rhs)' will always succeed",
      anchor: range)
  }

  static func useOfRValueAsLValue(expr: Node) -> Diag {
    return Diag(
      .error,
      "cannot use expression as an l-value",
      anchor: expr.range)
  }

  static func bindingWithNoUse(decl: ValueDecl) -> Diag {
    return Diag(
      .warning,
      "'\(decl.name)' is never used",
      anchor: decl.range)
  }

}
