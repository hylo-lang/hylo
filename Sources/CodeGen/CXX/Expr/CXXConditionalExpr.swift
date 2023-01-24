import Core

/// A C++ conditional expression
struct CXXConditionalExpr: CXXExpr {

  /// The condition expression.
  let condition: CXXExpr
  /// The expression to be returned if the condition is `true`.
  let trueExpr: CXXExpr
  /// The expression to be returned if the condition is `false`.
  let falseExpr: CXXExpr

  /// The original node in Val AST.
  let original: CondExpr.Typed

  func precedence() -> Int {
    16
  }
  func isLeftToRight() -> Bool {
    false
  }

}
