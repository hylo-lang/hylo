import Core

/// A C++ conditional expression
struct CXXConditionalExpr: CXXNode {

  /// The condition expression.
  public let condition: CXXNode
  /// The expression to be returned if the condition is `true`.
  public let trueExpr: CXXNode
  /// The expression to be returned if the condition is `false`.
  public let falseExpr: CXXNode

  /// The original node in Val AST.
  let original: CondExpr.Typed

  func writeCode<Target: TextOutputStream>(into target: inout Target) {
    condition.writeCode(into: &target)
    target.write(" ? ")
    trueExpr.writeCode(into: &target)
    target.write(" : ")
    falseExpr.writeCode(into: &target)
  }

}
