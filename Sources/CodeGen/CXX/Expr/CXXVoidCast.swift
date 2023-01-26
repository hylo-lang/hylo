import Core

/// Cast the result of an expression to void
struct CXXVoidCast: CXXExpr {

  let baseExpr: CXXExpr

  /// The original node in Val AST.
  let original: AnyExprID.TypedNode?

  var precedence: Int {
    3
  }
  var isLeftToRight: Bool {
    false
  }

}
