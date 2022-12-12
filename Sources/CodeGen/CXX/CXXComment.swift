import Core

/// A C++ comment
struct CXXComment: CXXRepresentable {

  /// The content of the comment.
  /// If the string contains a newline, this would be written as a multiline comment.
  public let comment: String

  /// The original node in Val AST.
  /// This node can be of any type.
  let original: AnyNodeID.TypedNode?

  /// Construct the CXX comment from the comment string, and optionaly the original AST node.
  init<ID: NodeIDProtocol>(_ comment: String, for original: TypedNode<ID>? = nil) {
    self.comment = comment
    if let orig = original {
      self.original = orig as? AnyNodeID.TypedNode
    } else {
      self.original = nil
    }
  }

  func writeCode<Target: TextOutputStream>(into target: inout Target) {
    if comment.contains("\n") {
      target.write("/* \(comment) */")
    } else {
      target.write("// \(comment)\n")
    }
  }

}
