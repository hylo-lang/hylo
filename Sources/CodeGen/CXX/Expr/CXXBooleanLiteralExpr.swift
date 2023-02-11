import Core

/// A C++ integer literal expression.
struct CXXBooleanLiteralExpr: CXXExpr {

  /// The value of the literal.
  let value: Bool

  var precedence: Int {
    0
  }
  var isLeftToRight: Bool {
    true  // doesn't really matter
  }

}
