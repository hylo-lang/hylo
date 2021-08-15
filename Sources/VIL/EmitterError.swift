import AST

/// An error that occured during VIL code emission.
public enum EmitterError: Error {

  /// An error denoting an assignment to an immutable (i.e., `val`) binding.
  case immutableBinding(ValueDecl)

  case immutableLocation

  case immutableSelf

  case immutableExpr

}
