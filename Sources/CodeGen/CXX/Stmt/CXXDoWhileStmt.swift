import Core

/// A C++ `do`-`while` statement
struct CXXDoWhileStmt: CXXStmt {

  /// The statement to be executed as part of the loop.
  let body: CXXStmt

  /// The expression to be tested at the end of the loop.
  let condition: CXXExpr

}
