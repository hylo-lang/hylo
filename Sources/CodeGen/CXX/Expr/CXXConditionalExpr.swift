import Core

/// A C++ conditional expression
struct CXXConditionalExpr: CXXExpr {

  /// The condition expression.
  let condition: CXXExpr
  /// The expression to be returned if the condition is `true`.
  let trueExpr: CXXExpr
  /// The expression to be returned if the condition is `false`.
  let falseExpr: CXXExpr

  var precedence: Int {
    16
  }
  var isLeftToRight: Bool {
    false
  }

}
