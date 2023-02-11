/// A type expression in C++.
struct CXXTypeExpr: CXXExpr {

  /// The textual represention of the type expression.
  let text: String

  /// True if the type is a native C++ type.
  let isNative: Bool

  /// Creates a C++ type expression from its textual representation.
  init(_ text: String) {
    self.text = text
    self.isNative = false
  }

  /// Creates a native C++ type expression from its textual representation.
  init(native text: String) {
    self.text = text
    self.isNative = true
  }

  var precedence: Int {
    0
  }
  var isLeftToRight: Bool {
    true  // doesn't really matter
  }

}
