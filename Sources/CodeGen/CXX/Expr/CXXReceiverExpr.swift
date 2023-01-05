import Core

/// A C++ `this` expression.
struct CXXReceiverExpr: CXXRepresentable {

  /// The original node in Val AST.
  let original: AnyExprID.TypedNode?

  func writeCode<Target: TextOutputStream>(into target: inout Target) {
    target.write("this")
  }

}
