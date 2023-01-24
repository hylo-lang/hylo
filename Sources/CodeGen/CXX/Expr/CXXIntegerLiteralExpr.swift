import Core

/// A C++ integer literal expression.
struct CXXIntegerLiteralExpr: CXXExpr {

  /// The value of the literal.
  let value: String

  /// The original node in Val AST.
  let original: IntegerLiteralExpr.Typed?

  func precedence() -> Int {
    0
  }
  func isLeftToRight() -> Bool {
    true  // doesn't really matter
  }

}
