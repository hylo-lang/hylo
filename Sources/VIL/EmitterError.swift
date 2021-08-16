import AST

/// An error that occured during VIL code emission.
public enum EmitterError: Error {

  /// The merror attempted to use an immutable binding in a mutable context.
  case immutableBinding(ValueDecl)

  /// The emitter attempted to treat an immutable reference to `self` in a mutable context.
  case immutableSelf(property: ValueDecl)

  /// The emitter attempted to treat an immutable expression in a mutable context.
  case immutableExpr

}
