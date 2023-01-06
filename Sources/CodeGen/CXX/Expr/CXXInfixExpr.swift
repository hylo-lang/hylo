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

  func writeCode<Target: TextOutputStream>(into target: inout Target) {
    lhs.writeCode(into: &target)
    target.write(" ")
    callee.writeCode(into: &target)
    target.write(" ")
    rhs.writeCode(into: &target)
  }

}
