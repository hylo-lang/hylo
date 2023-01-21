import Core

/// A C++ `while` statement
struct CXXWhileStmt: CXXStmt {

  /// The expression to be tested by the `while` loop.
  let condition: CXXNode

  /// The statement to be executed as part of the loop.
  let body: CXXNode

  /// The original node in Val AST.
  /// This node can be of any type.
  let original: AnyNodeID.TypedNode?

  func writeCode<Target: TextOutputStream>(into target: inout Target) {
    target.write("while ( ")
    condition.writeCode(into: &target)
    target.write(" ) ")
    body.writeCode(into: &target)
  }

}
