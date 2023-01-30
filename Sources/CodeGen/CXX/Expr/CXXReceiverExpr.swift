import Core

/// A C++ `this` expression.
struct CXXReceiverExpr: CXXExpr {

  /// The original node in Val AST.
  let original: AnyExprID.TypedNode?

  var precedence: Int {
    0
  }
  var isLeftToRight: Bool {
    true  // doesn't really matter
  }

}
