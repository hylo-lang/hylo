import Core

/// A C++ scoped block -- multiple statements in curly braces
struct CXXScopedBlock: CXXRepresentable {

  /// The statements in the scoped block.
  public let stmts: [CXXRepresentable]

  /// The original node in Val AST.
  /// This node can be of any type.
  let original: AnyNodeID.TypedNode?

  /// Construct the CXX scoped block from the list of CXX entities, and optionaly the original AST node.
  init<ID: NodeIDProtocol>(_ stmts: [CXXRepresentable], for original: TypedNode<ID>? = nil) {
    self.stmts = stmts
    if let orig = original {
      self.original = orig as? AnyNodeID.TypedNode
    } else {
      self.original = nil
    }
  }
  func writeCode<Target: TextOutputStream>(into target: inout Target) {
    target.write("{\n")
    for stmt in stmts {
      stmt.writeCode(into: &target)
    }
    target.write("}\n")
  }

}
