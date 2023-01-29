/// A type expression in C++.
struct CXXTypeExpr: CXXExpr {

  /// The textual represention of the type expression.
  let code: String

  /// Creates a C++ type expression from its textual representation.
  init(_ code: String) {
    self.code = code
  }

  var precedence: Int {
    0
  }
  var isLeftToRight: Bool {
    true  // doesn't really matter
  }

}
