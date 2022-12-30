import Core

/// A C++ integer literal expression.
public struct CXXIntegerLiteralExpr: CXXRepresentable {

  /// The value of the literal.
  let value: String

  /// The original node in Val AST.
  let original: AnyExprID.TypedNode?

  /// Construct the integer literal expression from the original AST node.
  init<ID: NodeIDProtocol>(_ value: String, original: TypedNode<ID>? = nil) {
    self.value = value
    if let orig = original {
      self.original = orig as? AnyExprID.TypedNode
    } else {
      self.original = nil
    }
  }

  public func writeCode<Target: TextOutputStream>(into target: inout Target) {
    target.write(value)
  }

}
