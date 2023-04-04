import Core

/// A constructor of a C++ class.
struct CXXConstructor: CXXDecl {

  /// A parameter in a C++ constructor declaration.
  typealias Parameter = (name: CXXIdentifier, type: CXXTypeExpr)

  /// An initializer for the class.
  typealias Initializer = (name: CXXIdentifier, value: CXXExpr)

  /// The name of the class containing `self`.
  let name: CXXIdentifier

  /// The parameters of the constructor.
  let parameters: [Parameter]

  /// The initializers of the constructor.
  let initializers: [Initializer]

  /// The body of the function.
  let body: CXXStmt?

}
