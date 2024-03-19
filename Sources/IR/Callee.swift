import Core

/// The callee of a function call in the IR.
enum Callee {

  /// A direct reference to a function, initializer, or method implementation.
  case direct(FunctionReference)

  /// A reference to a variant in a method bundle.
  case bundle(BundleReference<MethodDecl>)

  /// A lambda.
  case lambda(Operand)

}
