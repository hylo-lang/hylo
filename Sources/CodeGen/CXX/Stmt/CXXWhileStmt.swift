import Core

/// A C++ `while` statement
struct CXXWhileStmt: CXXStmt {

  /// The expression to be tested by the `while` loop.
  let condition: CXXExpr

  /// The statement to be executed as part of the loop.
  let body: CXXStmt

  /// The original node in Val AST.
  /// This node can be of any type.
  let original: AnyNodeID.TypedNode?

}
