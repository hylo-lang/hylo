import Core

/// A C++ return statement
struct CXXReturnStmt: CXXStmt {

  /// The expression to be returned.
  let expr: CXXExpr?

  /// The original node in Val AST.
  /// This node can be of any type.
  let original: AnyNodeID.TypedNode?

}
