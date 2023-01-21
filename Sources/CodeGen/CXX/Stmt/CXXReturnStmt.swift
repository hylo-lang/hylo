import Core

/// A C++ return statement
struct CXXReturnStmt: CXXStmt {

  /// The expression to be returned.
  let expr: CXXNode?

  /// The original node in Val AST.
  /// This node can be of any type.
  let original: AnyNodeID.TypedNode?

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
