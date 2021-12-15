import AST
import Basic

extension Diag {

  // MARK: VILGen

  static func runtimeFunctionTypeConversion(range: SourceRange?) -> Diag {
    return Diag(
      "runtime conversion of function types is not supported; cast will always fail",
      level: .warning,
      anchor: range)
  }

  static func immutableBindingRequiresInitializer(decl: PatternBindingDecl) -> Diag {
    return Diag("local immutable binding requires an initializer", anchor: decl.range)
  }

  static func dynamicCastAlwaysFails(
    from lhs: ValType,
    to rhs: ValType,
    range: SourceRange?
  ) -> Diag {
    return Diag(
      "dynamic cast from '\(lhs)' to '\(rhs)' will always fail",
      level: .warning,
      anchor: range)
  }

  static func dynamicCastAlwaysSucceeds(
    from lhs: ValType,
    to rhs: ValType,
    range: SourceRange?
  ) -> Diag {
    return Diag(
      "dynamic cast from '\(lhs)' to '\(rhs)' will always succeed",
      level: .warning,
      anchor: range)
  }

  static func useOfRValueAsLValue(expr: Node) -> Diag {
    return Diag("cannot use expression as an l-value", anchor: expr.range)
  }

  static func bindingWithNoUse(decl: ValueDecl) -> Diag {
    return Diag("'\(decl.name)' is never used", level: .warning, anchor: decl.range)
  }

}
