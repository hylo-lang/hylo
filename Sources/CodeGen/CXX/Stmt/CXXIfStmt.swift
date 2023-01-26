import Core

/// A C++ conditional statement
struct CXXIfStmt: CXXStmt {

  /// The expression to be tested by the conditional.
  let condition: CXXExpr

  /// The statement to be executed if the condition is true.
  let trueStmt: CXXStmt
  /// The statement to be executed if the condition is false.
  /// Can be missing.
  let falseStmt: CXXStmt?

  /// The original node in Val AST.
  /// This node can be of any type.
  let original: AnyNodeID.TypedNode?

}
