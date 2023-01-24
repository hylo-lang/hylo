import Core

/// A C++ comment
struct CXXComment: CXXNode, CXXExpr, CXXStmt {

  /// The content of the comment.
  /// If the string contains a newline, this would be written as a multiline comment.
  let comment: String

  /// The original node in Val AST.
  /// This node can be of any type.
  let original: AnyNodeID.TypedNode?

}

// Just to conform with CXXExpr
extension CXXComment {

  func precedence() -> Int {
    0
  }
  func isLeftToRight() -> Bool {
    true
  }

}