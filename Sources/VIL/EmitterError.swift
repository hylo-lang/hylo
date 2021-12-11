import AST
import Basic

/// An error that occured during VIL code emission.
public enum EmitterError: Error {

  /// The emitter attempted to use an immutable capture in a mutable context.
  case immutableCapture(ValueDecl)

  /// The emitter attempted to extract a non-copyable stored property out of a record.
  case nonCopyableProperty(VarDecl)

  /// The emitter attempted to use an r-value as an l-value.
  case useOfRValueAsLValue(Expr)

  func diag() -> Diag {
    switch self {
    case .useOfRValueAsLValue(let expr):
      return .useOfRValueAsLValue(expr: expr)

    default:
      fatalError()
    }
  }

}
