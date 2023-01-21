import Core

/// A C++ local variable declaration.
struct CXXLocalVarDecl: CXXDecl, CXXStmt {

  /// The type of the local variable.
  let type: CXXTypeExpr
  /// The name of the local variable.
  let name: CXXIdentifier
  /// The initializer of the local variable.
  let initializer: CXXExpr?

  /// The original node in Val AST.
  let original: VarDecl.Typed

  func writeCode<Target: TextOutputStream>(into target: inout Target) {
    target.write("\(type) ")
    name.writeCode(into: &target)
    if let value = initializer {
      target.write(" = ")
      value.writeCode(into: &target)
    }
    target.write(";\n")
  }

}
