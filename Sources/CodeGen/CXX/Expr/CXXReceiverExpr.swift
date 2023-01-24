import Core

/// A C++ `this` expression.
struct CXXReceiverExpr: CXXExpr {

  /// The original node in Val AST.
  let original: AnyExprID.TypedNode?

  func precedence() -> Int {
    0
  }
  func isLeftToRight() -> Bool {
    true  // doesn't really matter
  }

}
