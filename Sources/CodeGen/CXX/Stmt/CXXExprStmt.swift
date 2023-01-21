import Core

/// A C++ expression statement.
struct CXXExprStmt: CXXStmt {

  /// The expression contained in this statement.
  let expr: CXXNode

  /// The original node in Val AST.
  let original: AnyNodeID.TypedNode

  func writeCode<Target: TextOutputStream>(into target: inout Target) {
    expr.writeCode(into: &target)
    target.write(";\n")
  }

}
