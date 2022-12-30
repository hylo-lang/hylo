import Core

/// A C++ return statement
struct CXXReturnStmt: CXXRepresentable {

  /// The expression to be returned.
  let expr: CXXRepresentable?

  /// The original node in Val AST.
  /// This node can be of any type.
  let original: AnyNodeID.TypedNode?

  /// Construct the CXX scoped block from the list of CXX entities, and optionaly the original AST node.
  init<ID: NodeIDProtocol>(_ expr: CXXRepresentable?, for original: TypedNode<ID>? = nil) {
    self.expr = expr
    if let orig = original {
      self.original = orig as? AnyNodeID.TypedNode
    } else {
      self.original = nil
    }
  }
  func writeCode<Target: TextOutputStream>(into target: inout Target) {
    if expr != nil {
      target.write("return ")
      expr!.writeCode(into: &target)
      target.write(";\n")
    } else {
      target.write("return;\n")
    }
  }

}
