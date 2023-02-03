import Core

/// A C++ comment
struct CXXComment: CXXNode, CXXExpr, CXXStmt {

  /// The content of the comment.
  /// If the string contains a newline, this would be written as a multiline comment.
  let comment: String

}

// Just to conform with CXXExpr
extension CXXComment {

  var precedence: Int {
    0
  }
  var isLeftToRight: Bool {
    true
  }

}
