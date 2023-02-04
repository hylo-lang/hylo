import Core

/// A C++ `while` statement
struct CXXWhileStmt: CXXStmt {

  /// The expression to be tested by the `while` loop.
  let condition: CXXExpr

  /// The statement to be executed as part of the loop.
  let body: CXXStmt

}
