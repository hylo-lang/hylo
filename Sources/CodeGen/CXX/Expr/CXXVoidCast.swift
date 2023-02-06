import Core

/// Cast the result of an expression to void
struct CXXVoidCast: CXXExpr {

  let baseExpr: CXXExpr

  var precedence: Int {
    3
  }
  var isLeftToRight: Bool {
    false
  }

}
