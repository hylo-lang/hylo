import Core

/// A C++ postfix operator call expression.
struct CXXPostfixExpr: CXXExpr {

  enum Operator {
    // operator                   example     precedence  associativity
    case suffixIncrement  //      a++         2           -->
    case suffixDecrement  //      a--         2           -->
  }

  /// The operator that needs to be called.
  let oper: Operator

  /// The base expression of the operator call.
  let base: CXXExpr

  /// The original node in Val AST.
  /// Typically an expression, but somtimes this can be AssignStmt
  let original: AnyNodeID.TypedNode?

  func precedence() -> Int {
    2
  }
  func isLeftToRight() -> Bool {
    true
  }

}
