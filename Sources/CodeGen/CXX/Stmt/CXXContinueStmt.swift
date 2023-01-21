import Core

/// A C++ `continue` statement
struct CXXContinueStmt: CXXNode {

  /// The original node in Val AST.
  /// This node can be of any type.
  let original: AnyNodeID.TypedNode?

  func writeCode<Target: TextOutputStream>(into target: inout Target) {
    target.write("continue;\n")
  }

}
