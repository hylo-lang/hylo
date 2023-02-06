import Core

/// A C++ local variable declaration.
struct CXXLocalVarDecl: CXXDecl, CXXStmt {

  /// The type of the local variable.
  let type: CXXTypeExpr
  /// The name of the local variable.
  let name: CXXIdentifier
  /// The initializer of the local variable.
  let initializer: CXXExpr?

}
