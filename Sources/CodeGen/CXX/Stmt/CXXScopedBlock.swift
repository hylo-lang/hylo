import Core

/// A C++ scoped block -- multiple statements in curly braces
struct CXXScopedBlock: CXXStmt {

  /// The statements in the scoped block.
  public let stmts: [CXXStmt]

  /// The original node in Val AST.
  /// This node can be of any type.
  let original: AnyNodeID.TypedNode?

  func writeCode<Target: TextOutputStream>(into target: inout Target) {
    target.write("{\n")
    for stmt in stmts {
      stmt.writeCode(into: &target)
    }
    target.write("}\n")
  }

}
