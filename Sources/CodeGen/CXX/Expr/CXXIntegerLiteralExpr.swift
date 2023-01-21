import Core

/// A C++ integer literal expression.
public struct CXXIntegerLiteralExpr: CXXNode {

  /// The value of the literal.
  let value: String

  /// The original node in Val AST.
  let original: IntegerLiteralExpr.Typed?

  public func writeCode<Target: TextOutputStream>(into target: inout Target) {
    target.write(value)
  }

}
