import Core

/// A C++ `this` expression.
struct CXXReceiverExpr: CXXExpr {

  var precedence: Int {
    0
  }
  var isLeftToRight: Bool {
    true  // doesn't really matter
  }

}
