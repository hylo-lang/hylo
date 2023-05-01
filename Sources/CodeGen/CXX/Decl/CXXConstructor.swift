import Core

/// A constructor of a C++ class.
struct CXXConstructor: CXXDecl {

  /// An initializer for the class.
  typealias Initializer = (name: CXXIdentifier, value: CXXExpr)

  /// The name of the class containing `self`.
  let name: CXXIdentifier

  /// The parameters of the constructor.
  let parameters: [CXXParameter]

  /// The initializers of the constructor.
  let initializers: [Initializer]

  /// The body of the function.
  let body: CXXStmt?

}
