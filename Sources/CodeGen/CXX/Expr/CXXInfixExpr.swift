import Core

/// A C++ infix operator call expression.
struct CXXInfixExpr: CXXExpr {

  /// The calee expression.
  /// Usually, just an identifier.
  let callee: CXXExpr

  /// The left-hand-side argument of the operator call.
  let lhs: CXXExpr

  /// The right-hand-side argument of the operator call.
  let rhs: CXXExpr

  /// The original node in Val AST.
  /// Typically an expression, but somtimes this can be AssignStmt
  let original: AnyNodeID.TypedNode?

}
