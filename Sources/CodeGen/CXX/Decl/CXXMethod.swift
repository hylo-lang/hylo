import Core

/// A method of a C++ class.
struct CXXMethod: CXXDecl {

  /// A parameter in a C++ constructor declaration.
  typealias Parameter = (name: CXXIdentifier, type: CXXTypeExpr)

  /// The name of the method.
  let name: CXXIdentifier

  /// The result type of the method.
  let resultType: CXXTypeExpr

  /// The parameters of the constructor.
  let parameters: [Parameter]

  /// True if `self` is a static class method.
  let isStatic: Bool

  /// The body of the function.
  let body: CXXStmt?

}
