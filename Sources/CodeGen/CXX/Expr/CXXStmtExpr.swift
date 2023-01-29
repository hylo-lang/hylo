import Core

/// A C++ statement that need to be represented as an expression.
struct CXXStmtExpr: CXXExpr {

  /// The statment contained in the expression.
  let stmt: CXXStmt

  /// The original node in Val AST.
  let original: AnyNodeID.TypedNode

  var precedence: Int {
    0
  }
  var isLeftToRight: Bool {
    true  // doesn't really matter
  }

}
