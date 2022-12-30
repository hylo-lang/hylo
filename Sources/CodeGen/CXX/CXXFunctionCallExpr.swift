import Core

/// A C++ function call expression.
struct CXXFunctionCallExpr: CXXRepresentable {

  /// The calee expression.
  /// Can be an identifier, compound expression or any complex expression/
  public let callee: CXXRepresentable

  /// The arguments of the function call.
  public let arguments: [CXXRepresentable]

  /// The original node in Val AST.
  let original: AnyExprID.TypedNode?

  /// Construct the CXX function call expression from the callee and arguments, and optionaly the original AST node.
  init<ID: NodeIDProtocol>(
    callee: CXXRepresentable, arguments: [CXXRepresentable], original: TypedNode<ID>? = nil
  ) {
    self.callee = callee
    self.arguments = arguments
    if let orig = original {
      self.original = orig as? AnyExprID.TypedNode
    } else {
      self.original = nil
    }
  }

  func writeCode<Target: TextOutputStream>(into target: inout Target) {
    callee.writeCode(into: &target)
    target.write("(")
    for (i, argument) in arguments.enumerated() {
      if i > 0 {
        target.write(", ")
      }
      argument.writeCode(into: &target)
    }
    target.write(")")
  }

}
