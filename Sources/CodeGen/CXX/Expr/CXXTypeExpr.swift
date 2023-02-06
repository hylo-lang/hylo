/// A type expression in C++.
struct CXXTypeExpr: CXXExpr {

  /// The textual represention of the type expression.
  let text: String

  /// Creates a C++ type expression from its textual representation.
  init(_ text: String) {
    self.text = text
  }

  var precedence: Int {
    0
  }
  var isLeftToRight: Bool {
    true  // doesn't really matter
  }

}
