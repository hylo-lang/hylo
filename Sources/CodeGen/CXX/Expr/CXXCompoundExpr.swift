import Core

/// A C++ compound expression; i.e., A.B
struct CXXCompoundExpr: CXXExpr {

  /// The base expression; i.e., the thing before the dot.
  let base: CXXExpr

  /// The identifier of the compound expression; i.e., the thing after the dot.
  /// Typically this is a CXXIdentifier.
  let id: CXXExpr

  /// The original node in Val AST.
  let original: AnyExprID.TypedNode?

  func precedence() -> Int {
    2
  }
  func isLeftToRight() -> Bool {
    true
  }

}
