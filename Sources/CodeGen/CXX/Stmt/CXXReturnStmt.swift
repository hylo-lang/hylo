import Core

/// A C++ return statement
struct CXXReturnStmt: CXXStmt {

  /// The expression to be returned.
  let expr: CXXExpr?

}
