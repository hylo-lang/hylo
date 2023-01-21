import Core

/// Cast the result of an expression to void
struct CXXVoidCast: CXXExpr {

  let baseExpr: CXXExpr

  /// The original node in Val AST.
  let original: AnyExprID.TypedNode?

  func writeCode<Target: TextOutputStream>(into target: inout Target) {
    target.write("(void) ")
    baseExpr.writeCode(into: &target)
  }

}
