import Core

/// A C++ integer literal expression.
struct CXXIntegerLiteralExpr: CXXExpr {

  /// The value of the literal.
  let value: String

  var precedence: Int {
    0
  }
  var isLeftToRight: Bool {
    true  // doesn't really matter
  }

}
