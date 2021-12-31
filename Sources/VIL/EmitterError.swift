import AST

/// An error that occured during VIL code emission.
public enum EmitterError: Error {

  /// The given immutable binding was used in a mutable context.
  case immutableBinding(ValueDecl)

  /// The given immutable capture was used in a mutable context.
  case immutableCapture(ValueDecl)

  /// An immutable reference to the given property of `self` was used in a
  /// mutable context.
  case immutableSelf(property: ValueDecl)

  /// An immutable expression was used in a mutable context.
  case immutableExpr

}
