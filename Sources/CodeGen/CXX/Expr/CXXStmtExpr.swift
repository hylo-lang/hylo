import Core

/// A C++ statement that need to be represented as an expression.
struct CXXStmtExpr: CXXExpr {

  /// The statment contained in the expression.
  let stmt: CXXStmt

  /// The original node in Val AST.
  let original: AnyNodeID.TypedNode

  func writeCode<Target: TextOutputStream>(into target: inout Target) {
    stmt.writeCode(into: &target)
  }

}
