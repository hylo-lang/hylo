import Core

/// A C++ comment
struct CXXComment: CXXNode {

  /// The content of the comment.
  /// If the string contains a newline, this would be written as a multiline comment.
  public let comment: String

  /// The original node in Val AST.
  /// This node can be of any type.
  let original: AnyNodeID.TypedNode?

  func writeCode<Target: TextOutputStream>(into target: inout Target) {
    if comment.contains("\n") {
      target.write("/* \(comment) */")
    } else {
      target.write("// \(comment)\n")
    }
  }

}
