import Core

/// A C++ function call expression.
struct CXXFunctionCallExpr: CXXExpr {

  /// The calee expression.
  /// Can be an identifier, compound expression or any complex expression.
  public let callee: CXXNode

  /// The arguments of the function call.
  public let arguments: [CXXNode]

  /// The original node in Val AST.
  let original: AnyExprID.TypedNode?

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
