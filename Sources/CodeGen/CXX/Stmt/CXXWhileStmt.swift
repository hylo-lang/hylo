import Core

/// A C++ `while` statement
struct CXXWhileStmt: CXXRepresentable {

  /// The expression to be tested by the `while` loop.
  let condition: CXXRepresentable

  /// The statement to be executed as part of the loop.
  let body: CXXRepresentable

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
