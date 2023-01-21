import Core

/// A C++ integer literal expression.
struct CXXBooleanLiteralExpr: CXXExpr {

  /// The value of the literal.
  let value: Bool

  /// The original node in Val AST.
  let original: BooleanLiteralExpr.Typed?

  func writeCode<Target: TextOutputStream>(into target: inout Target) {
    target.write(value ? "true" : "false")
  }

}
