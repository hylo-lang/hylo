import Core

/// A C++ `break` statement
struct CXXBreakStmt: CXXNode {

  /// The original node in Val AST.
  /// This node can be of any type.
  let original: AnyNodeID.TypedNode?

  func writeCode<Target: TextOutputStream>(into target: inout Target) {
    target.write("break;\n")
  }

}
