import Core

/// A C++ infix operator call expression.
struct CXXInfixExpr: CXXRepresentable {

  /// The calee expression.
  /// Usually, just an identifier.
  public let callee: CXXRepresentable

  /// The left-hand-side argument of the operator call.
  public let lhs: CXXRepresentable

  /// The right-hand-side argument of the operator call.
  public let rhs: CXXRepresentable

  /// The original node in Val AST.
  let original: AnyExprID.TypedNode?

  /// Construct the CXX function call expression from the callee and arguments, and optionaly the original AST node.
  init<ID: NodeIDProtocol>(
    callee: CXXRepresentable, lhs: CXXRepresentable, rhs: CXXRepresentable, original: TypedNode<ID>? = nil
  ) {
    self.callee = callee
    self.lhs = lhs
    self.rhs = rhs
    if let orig = original {
      self.original = orig as? AnyExprID.TypedNode
    } else {
      self.original = nil
    }
  }

  func writeCode<Target: TextOutputStream>(into target: inout Target) {
    lhs.writeCode(into: &target)
    target.write(" ")
    callee.writeCode(into: &target)
    target.write(" ")
    rhs.writeCode(into: &target)
  }

}
