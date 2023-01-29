import Core

/// A C++ function call expression.
struct CXXFunctionCallExpr: CXXExpr {

  /// The calee expression.
  /// Can be an identifier, compound expression or any complex expression.
  let callee: CXXExpr

  /// The arguments of the function call.
  let arguments: [CXXExpr]

  /// The original node in Val AST.
  let original: AnyExprID.TypedNode?

  var precedence: Int {
    2
  }
  var isLeftToRight: Bool {
    true
  }

}
