import Core

/// A C++ conditional statement
struct CXXIfStmt: CXXStmt {

  /// The expression to be tested by the conditional.
  let condition: CXXNode

  /// The statement to be executed if the condition is true.
  let trueStmt: CXXNode
  /// The statement to be executed if the condition is false.
  /// Can be missing.
  let falseStmt: CXXNode?

  /// The original node in Val AST.
  /// This node can be of any type.
  let original: AnyNodeID.TypedNode?

  func writeCode<Target: TextOutputStream>(into target: inout Target) {
    target.write("if ( ")
    condition.writeCode(into: &target)
    target.write(" ) ")
    trueStmt.writeCode(into: &target)
    if falseStmt != nil {
      target.write("else ")
      falseStmt!.writeCode(into: &target)
    }
  }

}
