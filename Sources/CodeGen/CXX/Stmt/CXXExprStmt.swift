import Core

/// A C++ expression statement.
struct CXXExprStmt: CXXStmt {

  /// The expression contained in this statement.
  let expr: CXXExpr

  /// The original node in Val AST.
  let original: AnyNodeID.TypedNode

}
