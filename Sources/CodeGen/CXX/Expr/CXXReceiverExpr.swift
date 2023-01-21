import Core

/// A C++ `this` expression.
struct CXXReceiverExpr: CXXExpr {

  /// The original node in Val AST.
  let original: AnyExprID.TypedNode?

}
