import Core

/// A C++ `this` expression.
struct CXXThisExpr: CXXRepresentable {

  /// The original node in Val AST.
  let original: AnyExprID.TypedNode?

  /// Construct the `this` expression from the original AST node.
  init<ID: NodeIDProtocol>(original: TypedNode<ID>? = nil) {
    if let orig = original {
      self.original = orig as? AnyExprID.TypedNode
    } else {
      self.original = nil
    }
  }

  func writeCode<Target: TextOutputStream>(into target: inout Target) {
    target.write("this")
  }

}
