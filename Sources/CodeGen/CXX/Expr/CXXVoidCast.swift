import Core

/// Cast the result of an expression to void
struct CXXVoidCast: CXXExpr {

  let baseExpr: CXXExpr

  /// The original node in Val AST.
  let original: AnyExprID.TypedNode?

  func precedence() -> Int {
    3
  }
  func isLeftToRight() -> Bool {
    false
  }

}
