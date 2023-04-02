import Core

/// A C++ function declaration.
struct CXXFunctionDecl: CXXTopLevelDecl {

  /// The ID of a C++ function in its module.
  typealias ID = Int

  /// The identifier of the function.
  let identifier: CXXIdentifier

  /// The output type of the function.
  let output: CXXTypeExpr

  /// The parameters of the function.
  let parameters: [CXXParameter]

  /// The body of the function.
  let body: CXXStmt?

}
