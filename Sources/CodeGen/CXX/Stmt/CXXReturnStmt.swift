import Core

/// A C++ return statement
struct CXXReturnStmt: CXXRepresentable {

  /// The expression to be returned.
  let expr: CXXRepresentable?

  /// The original node in Val AST.
  /// This node can be of any type.
  let original: AnyExprID.TypedNode?

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
